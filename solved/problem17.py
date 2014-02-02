
import itertools
import pdb

numbers_to_strings = {
	'1'  : 'one',
	'2'  : 'two',
	'3'  : 'three',
	'4'  : 'four',
	'5'  : 'five',
	'6'  : 'six',
	'7'  : 'seven',
	'8'  : 'eight',
	'9'  : 'nine',
	'10' : 'ten',
	'11' : 'eleven',
	'12' : 'twelve',
	'13' : 'thirteen',
	'14' : 'fourteen',
	'15' : 'fifteen',
	'16' : 'sixteen',
	'17' : 'seventeen',
	'18' : 'eighteen',
	'19' : 'nineteen',
	'20' : 'twenty',
	'30' : 'thirty',
	'40' : 'forty',
	'50' : 'fifty',
	'60' : 'sixty',
	'70' : 'seventy',
	'80' : 'eighty',
	'90' : 'ninety',
}

def get_number_in_english(number):
	if len(number) == 1:
		return numbers_to_strings[number]

	if len(number) == 2:
		if (number[0] == '1') or (number[-1] == '0'):
			return numbers_to_strings[number]
		else:
			tens_str = numbers_to_strings[number[0] + '0']
			return tens_str + numbers_to_strings[number[1]]

	if len(number) == 3:
		hundreds_str = numbers_to_strings[number[0]] + 'hundred'
		if number[1:] == '00':
			return hundreds_str
		else:
			if number[1] == '0':
				remaining = get_number_in_english(number[2])
			else:
				remaining = get_number_in_english(number[1:])
			return hundreds_str + 'and' + remaining

	if len(number) == 4:
		return 'onethousand'

def get_letter_count(number):
	number_as_string = get_number_in_english(str(number))
	print number, number_as_string
	return len(number_as_string) #gets spaces

total_num_letters = 0
for i in xrange(1, 1000+1):
	total_num_letters += get_letter_count(i)
print total_num_letters


