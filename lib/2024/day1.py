l1, l2 = [], []
with open('inputs/2024/full/day1') as f:
    for line in f:
        nums = line.split()
        l1.append(int(nums[0]))
        l2.append(int(nums[1]))

l1.sort()
l2.sort()
answer = sum([abs(i1 - i2) for i1, i2 in zip(l1, l2)])
print(answer)