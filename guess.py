# import random as rd

# maxtimes = 5
# num = rd.randint(1,100)
# while maxtimes > 0:
#     try:
#         guess = int(input())
#         if guess < num :
#             print("太小")
#         elif guess > num :
#             print("太大")
#         else : 
#             print("对了")
#             break
#         maxtimes -= 1
#     except ValueError as e:
#         print("请输入数字")
# else:
#     print("不敢相信你竟然没猜出来")


import re ,random as rd
# maxtimes = 5
# num = rd.randint(1,100)

# numtextre = re.compile(r'^-?([1-9][0-9]*|[0-9])$')

# while maxtimes:
#     s = input().strip()
#     m = numtextre.fullmatch(s)
#     if m is None:
#         print("格式错误了")
#         continue
#     guess = int(m.group())
#     if guess < num :
#         print("太小")
#     elif guess > num :
#         print("太大")
#     else : 
#         print("对了")
#         break
#     maxtimes -= 1
# else:
#     print("不敢相信你竟然没猜出来")
import re ,random as rd
pos = rd.randint(0,3)
numtextre = re.compile(r'^([1-9][0-9]*|[0-9])$')
maxtimes = 5
while maxtimes > 0:
    s = input().strip()
    m = numtextre.fullmatch(s)
    if m is None:
        print("格式错误") 
        continue
    guess = int(m.group())
    if guess == pos :
        print("抓到了")
        break
    print("不对，它跑了")
    mov = rd.choice([-1,1])
    pos += mov + 4
    pos %= 4
    maxtimes -= 1
else :
    print("笨死了")

# text = r'123/123-4212-4242/23543'

# for item in re.split(r'/|-',text):
#     print(item)