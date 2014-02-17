
def increment(year, month, day):
	day += calc_num_days_in_month(year, month)

	month %= 12
	month += 1

	if month == 1:
		year += 1
	
	return (year, month, day)

def calc_num_days_in_month(year, month):
	if month == 1: return 31
	elif month == 2:
		if year % 4 == 0 and (year % 100 != 0 or year % 400 == 0):
			return 29
		else:
			return 28
	elif month == 3: return 31
	elif month == 4: return 30
	elif month == 5: return 31
	elif month == 6: return 30
	elif month == 7: return 31
	elif month == 8: return 31
	elif month == 9: return 30
	elif month == 10: return 31
	elif month == 11: return 30
	elif month == 12: return 31

num_sundays = 0
year = 1901
month = 1
day = 2 # Tuesday
while year < 2000:
	if day % 7 == 0:
		num_sundays += 1

	(year, month, day) = increment(year, month, day)

print num_sundays + 1 #off-by-one somewhere