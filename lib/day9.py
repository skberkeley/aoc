from math import comb

history=open('../inputs/day9').readlines()

def Lagrange1(nums):
    n=len(nums)
    res=0
    for i,x in enumerate(nums):
        res+=x*comb(n,i)*(-1)**(n-1-i)
    return res

res1 = 0
l = []
for line in history:
    nums=list(map(int,line.strip().split()))
    x = Lagrange1(nums)
    l.append(x)
    res1+=x
print(l)
print(res1)