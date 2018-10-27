t = (1,3)

t1 = (1,'a',"Hello")

arr2d = [[1,2,3],[2,3],[1,2,3,4]]

-- No !
-- arrt = [(1,2,3),(1,2),(1)]

-- two elem tuple
t2 = (1,2)
a = fst t2

arr = zip [1..10]  ['a'..]

triangle = [(a,b,c)| c<-[1..10],b<-[1..c],a<-[1..b], a*a + b*b == c*c]
triangle2 = [(a,b,c)| c<-[1..10],b<-[1..c],a<-[1..b], a*a + b*b == c*c,a+b+c==24]

