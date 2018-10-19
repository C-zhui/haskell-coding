import random as rd
arr = [rd.randint(1, 1000000) for i in range(100010)]


def qs(arr):  # 快速排序
    return arr if len(arr) <= 1 else qs([i for i in arr[1:] if i < arr[0]]) + [arr[0]] + qs([i for i in arr[1:] if i >= arr[0]])


''




















def qsg(arr):  # 生成器的 快速排序
    if len(arr) == 0:
        return
    if len(arr) == 1:
        yield arr[0]
        return
    m = arr[0]
    lt = [a for a in arr if a < m]
    yield from qsg(lt)

    eq = [a for a in arr if a == m]
    yield from eq

    gt = [a for a in arr if a > m]
    yield from qsg(gt)
    return

''

def take(n, iterable):
    i = 0
    res = []
    gen = iter(iterable)
    try:
        while i < n:
            i += 1
            res.append(next(gen))
    except StopIteration:
        pass
    finally:
        return res

''




import time

s = time.process_time_ns()
res1 = qs(arr)
print(time.process_time_ns()-s)

s = time.process_time_ns()
res2 = [*qsg(arr)]
print(time.process_time_ns()-s)


# import time
# s = time.process_time_ns()
# print(qs(arr)[:10])
# print(time.process_time_ns()-s)

# s = time.process_time_ns()
# print(take(10,qsg(arr)))
# print(time.process_time_ns()-s)
