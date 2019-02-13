import numpy as np
import pickle
import time
from math import pow
from math import ceil
from math import sqrt
import copy
import matplotlib.pyplot as plt
from scipy.spatial.distance import euclidean
from pulp import *
#from pyOpt import *
from pyOpt import Optimization

# from pyOpt import PSQP
# from pyOpt import pySNOPT
# from pyOpt import pyMMA
from pyOpt import SLSQP
from pyOpt import CONMIN
from pyOpt import COBYLA
# from pyOpt import SOLVOPT
# from pyOpt import KSOPT
# from pyOpt import NSGA2
# from pyOpt import ALGENCAN
# from pyOpt import FILTERSD
# from pyOpt import pyGCMMA
# from pyOpt import ALPSO
# from pyOpt import pyNLPQLP

# from mpi4py import MPI
import multiprocessing as mp

#concave function in 1<a<=2 and 1<b<=2
# and f(0)=f(1)=0
def beta_pdf(a,b,x):
	if x<=0:
		return 0.
	if x>=1:
		return 0.	
	return x*(1.-x)

def beta_pdf2(a,b,x):
	if x<0:
		x=0.0
	if x>1:
		x=1.0	
	e=0.00
	p= pow(x+e,a-1)*pow((1-(x+e)),(b-1))
	return p

def create_weights2(n,c):
	u = np.array([[0.0 for i in range(n)] for i in range(n)])
	zero = [0.0 for i in range(c)]
	one = [1.0 for i in range(c)]
	m = np.random.beta(2,2, (n,c)) #features
	w = np.random.beta(2,2,(n,c)) #features
	for i in range(n):
		for j in range(n):
			u[i,j] = euclidean(w[j],m[i])/euclidean(zero,one) # 1 - hamming_distance(w[j],m[i])
			u[i,j] = sqrt(u[i,j])
	return u


def hamming_distance(x,y):
	n = len(x)
	sim = 0.0
	for i in range(n):
		if x[i]==y[i]:
			sim+=1
	return sim/n


def create_weights(n):
	u = np.array([[0.0 for i in range(n)] for i in range(n)])
	m = np.random.beta(1,3, n) #features
	w = np.random.beta(5,1, n) #features
	for i in range(n):
		for j in range(n):
			if m[i]<=w[j]:
				u[i,j] = 1- 2*min((w[j]-m[i]),(m[i]-w[j]+1))
			else:
				u[i,j] = 1- 2*min((m[i]-w[j]),(w[j]-m[i]+1))
			u[i,j] = pow(u[i,j],1)
	return u

def create_weights3(n):
	u = np.array([[0.0 for i in range(n)] for i in range(n)])
	m = np.random.uniform(0,0.5, n) #features
	w = np.random.uniform(0.5,1, n) #features
	for i in range(n):
		for j in range(n):
			if m[i]<=w[j]:
				u[i,j] = 1- (w[j]-m[i])
			else:
				u[i,j] = 1- (m[i]-w[j])
			u[i,j] = pow(u[i,j],1)
	return u

def create_weights4(n):
	u = np.array([[0.0 for i in range(n)] for i in range(n)])
	for i in range(n):
		u[i] = np.random.beta(2,2, n)
	return u	

def create_weights5(n):
	u = np.array([[0.0 for i in range(n)] for i in range(n)])
	m = np.random.uniform(0,1, n) #features
	w = np.random.uniform(0,1, n) #features
	for i in range(n):
		for j in range(n):
			u[i,j] = m[i]*w[j]
		#u[i]= u[i]/max(u[i]) 
	u= u/max([max(u[i]) for i in range(n)])
	return u	

def create_weights6(n):
	u = np.array([[0.0 for i in range(n)] for i in range(n)])
	m = np.random.uniform(0,1, n) #features
	w = np.random.uniform(0,1, n) #features
	for i in range(n):
		for j in range(n):
			if m[i]<=w[j]:
				u[i,j] = (w[j]-m[i])
			else:
				u[i,j] = (m[i]-w[j])
			u[i,j] = pow(u[i,j],1)
	return u

def histogram_weights(u):
	n = len(u)
	h = []
	for i in range(n):
		for x in u[i]:
			h.append(x)
	h = sorted(h)
	plt.hist(h)
	plt.show()

def matching(n,m,a,b):
	# u = np.random.uniform(0,1,(n,n)) #utility
	# u = create_weights2(n,2) # # of dimensions for feature space
	u = create_weights4(n)
	# histogram_weights(u)
	# u =  np.random.beta(1,5, (n,m))
	#normalize
	# for i in range(n):
	# 	u[i]=u[i]/(max(u[i])+0.01)
	# u = np.random.beta(2,2,(n,n)) #utility
	u_sort=[]
	for i in range(n):
		u_sort.append(np.argsort(u[i])[::-1])
	#parameters of probability function
	a = np.random.uniform(1.5,2,m) #alpha
	b = np.random.uniform(1.5,2,m) #beta 
	M_opt = range(n) # Men available
	M_self = range(n) # Men available
	M_comp = range(n) # Men available
	W_opt = range(m) # Women available
	W_self = range(m) # Women available
	W_comp = range(m) # Women available
	
	couples_opt = [] # Couples match
	couples_self = [] # Couples match
	couples_comp= [] # Couples match
	couples_opt2 = [] # Couples match
	couples_self2 = [] # Couples match
	couples_comp2= [] # Couples match

	utility_opt1 = []
	utility_opt2 = []
	utility_self1 = []
	utility_self2 = []
	utility_comp1 = []
	utility_comp2 = []

	epsilon=0.05
	step = n
	# while M_opt:
	for i in xrange(n): 
		r = np.random.uniform(0,1,n)
		
		if M_opt:
			u1 = match_opt(M_opt[0],r,M_opt,W_opt,couples_opt,couples_opt2,u,u_sort,m,a,b)
			#print 'opt ',u1
			if (i+1)%step==0:
				u2 = date(M_opt,W_opt,couples_opt,u,a,b,r)
				utility_opt2.append(u2)
			if u1>=0:
				utility_opt1.append(u1)
		
		if M_self:
			u1 = match_selfish3(M_self[0],r,M_self,W_self,couples_self,couples_self2,u,u_sort,m,a,b)
			#print 'self ',u1
			if (i+1)%step==0:
				u2 = date(M_self,W_self,couples_self,u,a,b,r)
				utility_self2.append(u2)
			if u1>=0:
				utility_self1.append(u1)
		

		if M_comp:
			u1 = match_competetion(M_comp[0],r,M_comp,W_comp,couples_comp,couples_comp2,u,u_sort,m,a,b,epsilon)
			#print 'rand ', u1
			if (i+1)%step==0:
				u2 = date(M_comp,W_comp,couples_comp,u,a,b,r)
				utility_comp2.append(u2)
			if u1>=0:
				utility_comp1.append(u1)
		
		# if i+ini_size<n:
		# 	W_opt.append(i+ini_size)
		# 	W_self.append(i+ini_size)

		# print  u1, u2
	# print "optimal ", utility_opt
	# print "selfish ", utility_self
		# print len(couples_opt), len(couples_self)
		#print len(W_opt), len(W_self)
	return utility_opt1, utility_opt2, utility_self1,utility_self2, utility_comp1,utility_comp2, u
	# return utility_opt1, utility_opt2, utility_self1,utility_self2, u

def date(M,W,C,U,a,b,r):
	u1=0.0
	u2=0.0
	n = len(C)
	n2 = 0
	for i in range(n):
		x,y= C.pop()
		utility = U[x,y]
		u1 += utility
		q = beta_pdf(a[y],b[y],utility)
		if q > np.random.uniform():
			# Add men and women back to matching list
			M.append(x)
			W.append(y)
		else:
			# epsilon = (1.0-utility)/4.0
			# if q > beta_pdf(a[y],b[y],utility+epsilon):
			C.insert(0,(x,y))
			u2 += utility
			n2 += 1
	return u1/n
				

def date2(M,W,C,U,a,b,r):
	u1=0.0
	u2=0.0
	n = len(C)
	n2 = 0
	if C:
		x,y= C.pop()
		utility = U[x,y]
		u1 += utility
		q = beta_pdf(a[y],b[y],utility)
	else:
		return 0.0
	if q > np.random.uniform():
		# Add men and women back to matching list
		M.append(x)
		W.append(y)
	else:
		# epsilon = (1.0-utility)/4.0
		# if q > beta_pdf(a[y],b[y],utility+epsilon):
		# C.insert(0,(x,y))
		u2 += utility
		n2 += 1
	return u1/n

def match_opt(m,r,M,W,C,C2,u,u_sort,n,a,b):
	x = u_sort[m]
	utility = 0
	utility2 = 0
	for i in range(n):
		if x[i] in W and (m,x[i]) not in C2:
			utility = u[m,x[i]]
			C.append((m,x[i]))
			C2.append((m,x[i]))
			# print 'date_opt ', m,x[i]
			M.remove(m)
			W.remove(x[i])
			break
	# 		if q < r:
	# 			W.remove(x[i])
	# 			epsilon = (1-utility)/2
	# 			if q > beta_pdf(a[x[i]],b[x[i]],utility+epsilon):
	# 				utility2 = utility
			# break
	#print  beta_pdf(a[x[i]],b[x[i]],utility)	
	return utility


def match_rand(m,r,M,W,C,C2,u,u_sort,n,a,b):
	x = u_sort[m]
	utility = 0
	utility2 = 0
	for i in range(n):
		x =np.random.randint(0,n)
		if x in W  and (m,x) not in C2:
			utility = u[m,x]
			C.append((m,x))
			C2.append((m,x))
			# print 'date_opt ', m,x[i]
			M.remove(m)
			W.remove(x)
			break
	# 		if q < r:
	# 			W.remove(x[i])
	# 			epsilon = (1-utility)/2
	# 			if q > beta_pdf(a[x[i]],b[x[i]],utility+epsilon):
	# 				utility2 = utility
			# break	
	return utility


def match_selfish(m,r,W,u,u_sort,n,a,b):
	x = u_sort[m][::-1]
	max_q = 0.0
	max_index = -1
	utility = 0
	utility2 = 0
	for i in range(n):
		if x[i] in W  and (m,x[i]) not in C2:
			q = beta_pdf(a[x[i]],b[x[i]],u[m,x[i]])
			if max_q < q/(1+q):
				max_q = q/(1+q)
				max_index = x[i]
	if max_index >= 0:
		utility = u[m,max_index]
		q = beta_pdf(a[max_index],b[max_index],utility)
		if q < r:
			W.remove(max_index)
			epsilon = (1-utility)/2
			if q > beta_pdf(a[max_index],b[max_index],utility+epsilon):
				utility2 = utility		
	return utility,utility2


def match_selfish2(m,r,M,W,C,C2,u,u_sort,n,a,b,alpha):
	x = u_sort[m][::-1]
	max_u = 0.0
	max_index = -1
	utility = 0
	utility2 = 0
	for i in range(n):
		if x[i] in W  and (m,x[i]) not in C2:
			q = beta_pdf(a[x[i]],b[x[i]],u[m,x[i]])
			aux = (q/(1+q))*alpha + u[m,x[i]]*(1-alpha)
			if max_u < aux:
				max_u = aux
				max_index = x[i]
	if max_index >= 0:
		utility = u[m,max_index]
		# print m, max_index, utility
		C.append((m,max_index))
		C2.append((m,max_index))
		# print  beta_pdf(a[max_index],b[max_index],utility), utility
		# print 'date_self ', m,max_index
		M.remove(m)
		W.remove(max_index)
		
	# 	utility = u[m,max_index]
	# 	q = beta_pdf(a[max_index],b[max_index],utility)
	# 	if q < r:
	# 		W.remove(max_index)
	# 		epsilon = (1-utility)/2
	# 		if q > beta_pdf(a[max_index],b[max_index],utility+epsilon):
	# 			utility2 = utility	
	# print  beta_pdf(a[max_index],b[max_index],utility)		
	return utility

#match to one user according to distribution p
def get_match(p):
	n = len(p)
	r = np.random.uniform(0,1)
	c = 0.0
	for i in range(n):
		c = c + p[i]
		if r <= c:
			return i

def match_selfish3(m,r,M,W,C,C2,u,u_sort,n,a,b):
	max_u = 0.0
	max_index = -1
	utility = 0
	utility2 = 0
	mates = []
	for i in range(n):
		if i in W  and (m,i) not in C2:
			mates.append(i)
	p = online_solve(m,mates,u,n,a[m],b[m],objfunc_online_self_utility)
	mate = get_match(p)
	utility = u[m,mate]
	# print m, max_index, utility
	C.append((m,mate))
	C2.append((m,mate))
	# print  beta_pdf(a[max_index],b[max_index],utility), utility
	# print 'date_self ', m,max_index
	M.remove(m)
	W.remove(mate)
			
	return utility

def match_competetion(m,r,M,W,C,C2,u,u_sort,n,a,b,epsilon):
	max_u = 0.0
	max_index = -1
	utility = 0
	utility2 = 0
	mates = []
	for i in range(n):
		if i in W  and (m,i) not in C2:
			mates.append(i)
	p = online_solve_competition(m,mates,u,n,a[m],b[m],epsilon, objfunc_online_competition)
	mate = get_match(p)
	utility = u[m,mate]
	# print m, max_index, utility
	C.append((m,mate))
	C2.append((m,mate))
	# print  beta_pdf(a[max_index],b[max_index],utility), utility
	# print 'date_self ', m,max_index
	M.remove(m)
	W.remove(mate)
			
	return utility


def plot_utility(u,labels):
	N = len(u)
	n = len(u[0])
	x = range(n)

	colors =  ['r', 'g', 'b', 'y', 'm', 'c', 'r', 'g', 'b', 'y', 'm', 'c']
	markers =  ['o', '^', 's', 'h', 'D', '*','h', 'p', '*', 'D', 's', '^', 'o',]
	lines = ["-","--","-.",":"]

	for i in xrange(N):
		plt.scatter(x, u[i], color=colors[i], marker=markers[i], label=labels[i])
		
	plt.legend(loc=2, fontsize=16)
	# plt.title('Utility', fontsize=20) 
	plt.xlabel('Time', fontsize=16)
	plt.ylabel('Utility', fontsize=16)
	plt.show()


	# #save data
	# name = f+'/down'+ suffix + '_'+ str(N)+'_'+str(exps)+'.csv'
	# output = open(name, 'w')
	# output.write('x, ' + str(y).replace('[','').replace(']',''))
	# output.write('\n')
	# for i in xrange(n):
	# 	# slope, intercept = np.polyfit(samples, x[i], 1)
	# 	output.write(labels[i]+',')
	# 	# output.write('slope, ' + str(slope)+'\n')
	# 	output.write(str(x[i]).replace('[','').replace(']',''))
	# 	output.write('\n')
	# output.close()



def plot_cumulative_utility(u,labels):
	N = len(u)
	n = len(u[0])
	x = range(n)
	y = []
	for i in range(N):
		c =[u[i][0]]
		n = len(u[0])
		for j in range(1,n):
			c.append(c[j-1]+u[i][j])
		y.append(c)

	colors =  ['r', 'g', 'b', 'y', 'm', 'c', 'r', 'g', 'b', 'y', 'm', 'c']
	markers =  ['o', '^', 's', 'h', 'D', '*','h', 'p', '*', 'D', 's', '^', 'o',]
	lines = ["-","--","-.",":"]

	for i in xrange(N):
		plt.plot(x, y[i], color=colors[i], label=labels[i])
		
	plt.legend(loc=2, fontsize=16)
	# plt.title('Utility', fontsize=20) 
	plt.xlabel('Time', fontsize=16)
	plt.ylabel('Utility', fontsize=16)
	plt.show()

def plot_avg_utility(u,labels,u_static):
	N = len(u)
	N2= len(u_static)
	y = []
	for i in range(N):
		n = len(u[i])
		c =[u[i][0]]
		for j in range(1,n):
			c.append(c[j-1]+u[i][j])
		y.append(np.array(c)/np.array(range(1,len(c)+1)))


	colors =  ['r', 'g', 'b', 'y', 'm', 'c', 'r', 'g', 'b', 'y', 'm', 'c']
	markers =  ['o', '^', 's', 'h', 'D', '*','h', 'p', '*', 'D', 's', '^', 'o',]
	lines = ["-","--","-.",":"]

	for i in xrange(N):
		plt.plot(range(len(u[i])), y[i], color=colors[i], label=labels[i])
	#plot static values
	for i in xrange(N2):
		plt.plot(range(len(u[i])), [u_static[i] for j in range(len(u[i]))], color=colors[i], label=labels[i], linestyle= lines[1])
		
	plt.legend(loc=4, fontsize=16)
	# plt.title('Utility', fontsize=20) 
	plt.xlabel('Time', fontsize=16)
	plt.ylabel('Utility', fontsize=16)
	plt.show()



#Optimal offline models
def solve_LP(w,n,m,a,b):
	# Create the 'prob' variable to contain the problem data
	prob = LpProblem("Dating Problem", LpMaximize)
	x = LpVariable.dicts("X",(range(n),range(m)),lowBound=0, upBound=1.0)

	# The objective function is added to 'prob' first
	obj = []
	for i in range(n):
		ui = sum([w[i][j]*x[i][j] for j in range(m)])
		# qi = beta_pdf(a,b,ui)
		obj.append(ui)
	prob += lpSum(obj), "Utility"

	for i in range(n):
		prob += lpSum([x[i][j] for j in range(m)])<=1.0
	for i in range(m):
		prob += lpSum([x[j][i] for j in range(n)])<=1.0

	prob.solve()
	t = 0
	sol = [[0.0 for i in range(m)] for i in range(n)]
	for i in range(n):
		for j in range(m):
			if x[i][j].value() == 1.0:
				t+=1
				sol[i][j]= x[i][j].value()
				# print i,j
	return get_utility(sol,w)

def objfunc_self(xl,**kwargs):
	n = kwargs['n']
	a = kwargs['a']
	b = kwargs['b']
	w = [[0.0 for i in range(n)] for i in range(n)]
	for i in range(n):
		for j in range(n):
			w[i][j] = kwargs['w'][i][j]

	#transform list to matrix
	x = [[0.0 for i in range(n)] for i in range(n)]
	for i in range(n):
		for j in range(n):
			x[i][j]=xl[j+i*n]

	obj = []
	for i in range(n):
		ui = sum([w[i][j]*x[i][j] for j in range(n)])
		qi = beta_pdf(a,b,ui)
		obj.append(qi/(1+qi))
	f = sum(obj)
	g = []
	for i in range(n):
		g.append(sum([x[i][j] for j in range(n)])-1.0)
		g.append(sum([x[j][i] for j in range(n)])-1.0)

	fail = 0
  	return -1.0*f,g,fail


def objfunc_utility(xl,**kwargs):
	n = kwargs['n']
	a = kwargs['a']
	b = kwargs['b']
	w = [[0.0 for i in range(n)] for i in range(n)]
	for i in range(n):
		for j in range(n):
			w[i][j] = kwargs['w'][i][j]

	#transform list to matrix
	x = [[0.0 for i in range(n)] for i in range(n)]
	for i in range(n):
		for j in range(n):
			x[i][j]=xl[j+i*n]

	obj = 0.0
	for i in range(n):
		ui = sum([w[i][j]*x[i][j] for j in range(n)])
		obj += ui

	g = []
	for i in range(n):
		g.append(sum([x[i][j] for j in range(n)])-1.0)
		g.append(sum([x[j][i] for j in range(n)])-1.0)

	fail = 0
  	return -1.0*obj,g,fail

 

def solve(w,n,a,b,f):
	opt_prob = Optimization('Selfish Optimization',f)
	opt_prob.addObj('f')
	for i in range(n):
		for j in range(n):
			opt_prob.addVar('x'+str(i)+str(j),'c',lower=0.0,upper=1.0,value=0.0)
	for i in range(2*n):
		opt_prob.addCon('g'+str(i),'e')
	# print opt_prob


	# Instantiate Optimizer (SLSQP) & Solve Problem
	slsqp = SLSQP(pll_type=None) #"POA"
	# slsqp.setOption('IPRINT',-1)
	slsqp(opt_prob,sens_type='FD',sens_step=1e-3,  n=n, a=a, b=b, w=w)
	# print opt_prob.solution(0)

	# print ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
	x = [[0.0 for i in range(n)] for i in range(n)]
	for i in range(n):
		for j in range(n):
			x[i][j]=opt_prob.solution(0).getVar(j+i*n).value
	# print 'utility ', get_utility(x,w)

	return get_utility(x,w)



def solve_competition(w,n,a,b,e,f):
	opt_prob = Optimization('Selfish Optimization',f)
	opt_prob.addObj('f')
	for i in range(n):
		for j in range(n):
			opt_prob.addVar('x'+str(i)+str(j),'c',lower=0.0,upper=1.0,value=0.0)
	for i in range(2*n):
		opt_prob.addCon('g'+str(i),'i')
	# print opt_prob


	# Instantiate Optimizer (SLSQP) & Solve Problem
	slsqp = SLSQP() 
	# slsqp.setOption('IPRINT',-1)
	slsqp(opt_prob,sens_type='FD',MAXIT=200, n=n, a=a, b=b, epsilon=e, w=w)
	# print opt_prob.solution(0)

	

	# print ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
	x = [[0.0 for i in range(n)] for i in range(n)]
	for i in range(n):
		for j in range(n):
			x[i][j]=opt_prob.solution(0).getVar(j+i*n).value
	# print 'utility ', get_utility(x,w)

	return get_utility(x,w)


def objfunc_online_self_utility(x,**kwargs):
	n = kwargs['n']
	a = kwargs['a']
	b = kwargs['b']
	w = kwargs['w']
	mates = kwargs['mates']

	unavailble = list(set(range(n)) - set(mates))

	ui = sum([w[i]*x[i] for i in range(n)])
	qi = beta_pdf(a,b,ui)
	f = qi/(1+qi)


	g = []
	for i in unavailble:
		g.append(x[i]-0.0)	
	g.append(sum([x[i] for i in range(n)])-1.0)

	fail = 0
  	return -1.0*f,g,fail


def objfunc_competition(xl,**kwargs):
	n = kwargs['n']
	a = kwargs['a']
	b = kwargs['b']
	epsilon = kwargs['epsilon']
	w = [[0.0 for i in range(n)] for i in range(n)]
	for i in range(n):
		for j in range(n):
			w[i][j] = kwargs['w'][i][j]

	#transform list to matrix
	x = [[0.0 for i in range(n)] for i in range(n)]
	for i in range(n):
		for j in range(n):
			x[i][j]=xl[j+i*n]

	obj = []
	for i in range(n):
		ui = sum([w[i][j]*x[i][j] for j in range(n)])
		qi = beta_pdf(a,b,ui)
		obj.append(qi/(1+qi+qi*(1-ui)/epsilon))
	f = sum(obj)
	g = []
	for i in range(n):
		g.append(sum([x[i][j] for j in range(n)])-1.0)
		g.append(sum([x[j][i] for j in range(n)])-1.0)

	fail = 0
  	return -1.0*f,g,fail



def objfunc_online_competition(x,**kwargs):
	n = kwargs['n']
	a = kwargs['a']
	b = kwargs['b']
	w = kwargs['w']
	epsilon = kwargs['epsilon']
	mates = kwargs['mates']

	unavailble = list(set(range(n)) - set(mates))

	ui = sum([w[i]*x[i] for i in range(n)])
	qi = beta_pdf(a,b,ui)
	f = qi/(1+qi+qi*(1-ui)/epsilon)


	g = []
	for i in unavailble:
		g.append(x[i]-0.0)	
	g.append(sum([x[i] for i in range(n)])-1.0)

	fail = 0
  	return -1.0*f,g,fail



def online_solve(m,mates,w,n,a,b,f):
	opt_prob = Optimization('Selfish Online Optimization',f)
	opt_prob.addObj('f')


	for i in range(n):
		opt_prob.addVar('x'+str(i),'c',lower=0.0,upper=1.0,value=0.0)
	for i in range(n-len(mates)): # non available mates
		opt_prob.addCon('g'+str(i),'e')
	opt_prob.addCon('g','e') # match at most 1
	# print opt_prob


	# Instantiate Optimizer (SLSQP) & Solve Problem
	slsqp = SLSQP(pll_type=None) #"POA"
	# slsqp.setOption('IPRINT',-1)
	slsqp(opt_prob,sens_type='FD',sens_step=1e-6,  n=n, a=a, b=b, w=w[m], mates=mates)
	# print opt_prob.solution(0)

	# print ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
	x = [0.0 for i in range(n)]
	for i in range(n):
		x[i]=opt_prob.solution(0).getVar(i).value
	
	return x



def online_solve_competition(m,mates,w,n,a,b,epsilon,f):
	opt_prob = Optimization('Selfish Online Optimization',f)
	opt_prob.addObj('f')


	for i in range(n):
		opt_prob.addVar('x'+str(i),'c',lower=0.0,upper=1.0,value=0.0)
	for i in range(n-len(mates)): # non available mates
		opt_prob.addCon('g'+str(i),'e')
	opt_prob.addCon('g','e') # match at most 1
	# print opt_prob


	# Instantiate Optimizer (SLSQP) & Solve Problem
	slsqp = SLSQP(pll_type=None) #"POA"
	# slsqp.setOption('IPRINT',-1)
	slsqp(opt_prob,sens_type='FD',sens_step=1e-6,  n=n, a=a, b=b, w=w[m], epsilon=epsilon, mates=mates)
	# print opt_prob.solution(0)

	# print ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
	x = [0.0 for i in range(n)]
	for i in range(n):
		x[i]=opt_prob.solution(0).getVar(i).value
	
	return x


def get_utility(x,w):
	obj = 0.0
	n = len(x)
	m=len(x[0])
	for i in range(n):
		ui = sum([w[i][j]*x[i][j] for j in range(m)])
		obj += ui
	return obj
	# Instantiate Optimizer (SOLVOPT) & Solve Problem
	# solvopt = SOLVOPT()
	# solvopt.setOption('iprint',-1)
	# solvopt(opt_prob,sens_type='FD', n=n, a=a, b=b, w=w)
	# print opt_prob.solution(1)


def centralizedPoa(n,exps,e,x):
	np.random.seed(x)
	poa = []
	for i in range(exps):
		u = create_weights4(n)
		# a= np.random.uniform(1.5,2)
	 	# b= np.random.uniform(1.5,2)
	 	a = b = 2 
		epsilon = e 
		s = solve_competition(u,n,a,b,epsilon,objfunc_competition)
		# print "self", s
		opt = solve_LP(u,n,n,a,b)
		# print "LP", opt
		# opt = solve(u,n,a,b,objfunc_utility)
		# print "opt", opt

		poa.append(s/opt)

	# output.put(poa)
	return poa


def onlinePoa(n):

	u = create_weight4(n)
	a = np.random.uniform(1.5,2)
	b = np.random.uniform(1.5,2)
	u_opt1, u_opt2,u_self1,u_self2,u_rand1,u_rand2, w = matching(n,n,a,b)


def poa(epsilon):
	n=5
	exps = 100/4
	pool = mp.Pool(processes=4)
	results = [pool.apply(centralizedPoa, args=(n,exps,epsilon,x)) for x in range(4,8)]
	# results = centralizedPoa(n,5,1)

	# print(results)
	r=[]
	for x in results:
		r += x
	# print r
	mean = np.mean(r)
	max_poa =  max(r)
	min_poa = min(r)
	print epsilon,mean, max_poa, min_poa
	return mean, max_poa, min_poa


if __name__ == "__main__":
	mean_r=[]
	max_r =[]
	min_r =[]

	epsilon = [1.,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0.05,0.01,0.005]
	# epsilon = [1.,0.8,0.6,0.4,0.2,0.1,0.05,0.01,0.005]
	print "epsilon, mean, max, min"
	for e in epsilon:
		mean, max_poa, min_poa = poa(e)
		mean_r.append(mean)
		max_r.append(max_poa)
		min_r.append(min_poa)

	print(mean_r)
	print(max_r)
	print(min_r)

	# centralizedPoa(5,1,0.005,1)
	# n=4
	# u = create_weights6(n)
	# histogram_weights(u)

	


		
