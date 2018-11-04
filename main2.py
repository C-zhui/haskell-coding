a = [1,2,3]

for i in a:
    print(i)

# 等价于
ai = iter(a)
try:
    while True:
        i = next(ai)
        print(i)
except:
    pass