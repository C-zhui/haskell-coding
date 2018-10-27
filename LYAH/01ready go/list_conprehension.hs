arr = [x*2 | x<-[1..10]]

arr2 = [x*2 | x<-[1..10], x*2>10]

arr3 = [x|x<-[50..100],x `mod` 7==3]

boomBangs xs = [if x <10 then "Bang" else "Boom"|x<-xs,odd x]

arr4 = [x | x<-[10..20],x/=13,x/=15]

arr5 = [x*y | x<-[1..4],y<-[2..5]]

noun = ["I","You","Dog"]
verb = ["Go","Love","Eat"]

sen = [n ++ " " ++ v| n<-noun, v<-verb]

