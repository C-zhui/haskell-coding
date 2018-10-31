import random as rd
things = [1, 1, 0, 0]  # 1代表有奖，0代表无，不必分成4种
index = [0, 1, 2, 3]  # 门号
# 这个data用于统计，是个三维数组，实际上代表了选项分支
# 0 选择的不中 -- 0 打开的不中  -- 0  不坚持之后              中奖就+1，
#                             -- 1 坚持之后               因为不坚持有两种可能，结果/2
#              -- 1 打开的中奖 --0  不坚持之后
#                             -- 1 坚持之后
# 1 选择的中奖  -- 0 打开的不中 -- 0  不坚持之后
#                             -- 1 坚持之后
#              -- 1 打开的中奖 -- 0  不坚持之后
#                             -- 1 坚持之后
data = [
    [
        [0, 0] for i in range(2)
    ] for j in range(2)
]
i = 0
ALLTIMES = 1000000
while i < 1000000:
    i += 1
    rd.shuffle(things)  # 打乱
    first = rd.choice(index)  # 选择一个门
    rest = [a for a in index if a != first]

    opened = rd.choice(rest)  # 打开一个门
    data[things[first]][things[opened]][1] += 1 if things[first] == 1 else 0

    rest2 = [a for a in rest if a != opened]  # 不坚持就要看另外两个门
    data[things[first]][things[opened]][0] += (things[rest2[0]]+things[rest2[1]]) / 2

for i in range(len(data)):
    for j in range(len(data[i])):
        for k in range(len(data[i][j])):
            data[i][j][k] /= ALLTIMES

print(data)
