import pdb

def f(i, name):
	name = name.strip()
	alphs = [ord(ch) - ord('A') + 1 for ch in name]
	if i == 938:
		print i
		print alphs
		print i * (sum(alphs))

	return i * (sum(alphs))

names = []
for line in open("names.txt", "r"):
	names.append(line)

transformed = [f(i+1, name) for i, name in enumerate(names)]

print sum(transformed)