
# High Card: Highest value card.
# One Pair: Two cards of the same value.
# Two Pairs: Two different pairs.
# Three of a Kind: Three cards of the same value.
# Straight: All cards are consecutive values.
# Flush: All cards of the same suit.
# Full House: Three of a kind and a pair.
# Four of a Kind: Four cards of the same value.
# Straight Flush: All cards are consecutive values of same suit.
# Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

import euler
import pdb

RANK = 0
SUIT = 1

face_card_ranks = ['T', 'J', 'Q', 'K', 'A']

def obj_rankify(s):
	if s[RANK].isdigit():
		return int(s[RANK])
	else:
		return 10 + face_card_ranks.index(s[RANK])

def less(a, b):
	if a.isdigit():
		if b.isdigit():
			return a < b
		else:
			return True
	else:
		if b.isdigit():
			return False
		else:
			return face_card_ranks.index(a) < face_card_ranks.index(b)

def is_flush(hand):
	for card in hand:
		if card[SUIT] != hand[0][SUIT]:
			return False
	return True

def is_royal_flush(hand):
	return is_straight_flush(hand) and hand[0][RANK] == 'T'

def is_straight_flush(hand):
	return is_flush(hand) and is_straight(hand)

def is_straight(hand):
	for i in xrange(len(hand[1:]) + 1):
		if i == 0:
			i += 1
		if not obj_rankify(hand[i-1][RANK]) + 1 == obj_rankify(hand[i][RANK]):
			return False
	return True

def get_ranks_to_counts(hand):
	ranks_to_counts = {}
	for card in hand:
		if card[RANK] not in ranks_to_counts:
			ranks_to_counts[card[RANK]] = 0
		ranks_to_counts[card[RANK]] += 1
	return ranks_to_counts

def is_four_of_a_kind(hand):
	return is_n_of_a_kind(hand, 4)

def is_three_of_a_kind(hand):
	return is_n_of_a_kind(hand, 3)

def is_one_pair(hand):
	return is_n_of_a_kind(hand, 2)

def is_two_pairs(hand):
	return len(get_n_of_a_kind_elems(hand, 2)) == 2

def is_n_of_a_kind(hand, n):
	return len(get_n_of_a_kind_elems(hand, n)) != 0

def get_n_of_a_kind_elems(hand, n):
	ranks_to_counts = get_ranks_to_counts(hand)
	n_of_a_kind_elems = []
	for k, v in ranks_to_counts.items():
		if v == n:
			n_of_a_kind_elems.append(k)
	return n_of_a_kind_elems

def is_full_house(hand):
	pair = get_n_of_a_kind_elems(hand, 2)
	triple = get_n_of_a_kind_elems(hand, 3)

	return len(pair) != 0 and len(triple) != 0

def is_high_card(hand):
	return True

def calc_royal_flush_value(hand):
	hand.reverse()
	return (10, hand)

def calc_straight_flush_value(hand):
	hand.reverse()
	return (9, hand)

def calc_four_of_a_kind_value(hand):
	hand.reverse()
	return (8, hand)

def calc_full_house_value(hand):
	return (7, hand)

def calc_flush_value(hand):
	hand.reverse()
	return (6, hand)

def calc_straight_value(hand):
	hand.reverse()
	return (5, hand)

def calc_three_of_a_kind_value(hand):
	hand.reverse()
	return (4, hand)

def calc_two_pairs_value(hand):
	hand.reverse()
	return (3, hand)

def calc_one_pair_value(hand):
	hand.reverse()
	return (2, hand)

def calc_high_card_value(hand):
	hand.reverse()
	return (1, hand)

def get_hand_rank_as_string(hand):
	hand.sort(key=lambda x:obj_rankify(x[RANK]))

	if is_royal_flush(hand):       return 'royal flush'
	elif is_straight_flush(hand):  return 'straight flush'
	elif is_four_of_a_kind(hand):  return '4 of a kind'
	elif is_full_house(hand):      return 'full house'
	elif is_flush(hand):           return 'flush'
	elif is_straight(hand):        return 'straight'
	elif is_three_of_a_kind(hand): return '3 of a kind'
	elif is_two_pairs(hand):       return 'two pairs'
	elif is_one_pair(hand):        return 'one pair'
	elif is_high_card(hand):       return 'high card'

def get_hand_rank(hand):
	hand.sort(key=lambda x:obj_rankify(x[RANK]))

	if is_royal_flush(hand):       return calc_royal_flush_value(hand)
	elif is_straight_flush(hand):  return calc_straight_flush_value(hand)
	elif is_four_of_a_kind(hand):  return calc_four_of_a_kind_value(hand)
	elif is_full_house(hand):      return calc_full_house_value(hand)
	elif is_flush(hand):           return calc_flush_value(hand)
	elif is_straight(hand):        return calc_straight_value(hand)
	elif is_three_of_a_kind(hand): return calc_three_of_a_kind_value(hand)
	elif is_two_pairs(hand):       return calc_two_pairs_value(hand)
	elif is_one_pair(hand):        return calc_one_pair_value(hand)
	elif is_high_card(hand):       return calc_high_card_value(hand)
	else:
		return None

def compare_royal_flushes(a, b):
	return False

def compare_straight_flushes(a, b):
	return obj_rankify(a[0]) < obj_rankify(b[0])

def compare_fours_of_a_kind(a, b):
	return compare_ns_of_a_kind(a, b, 4)

def compare_ns_of_a_kind(a, b, n):
	a_n_of_a_kind_elems = get_n_of_a_kind_elems(a, n)
	b_n_of_a_kind_elems = get_n_of_a_kind_elems(b, n)

	return obj_rankify(a_n_of_a_kind_elems[0]) < obj_rankify(b_n_of_a_kind_elems[0])

def compare_full_houses(a, b):
	return compare_ns_of_a_kind(a, b, 3)

def compare_flushes(a, b):
	return obj_rankify(a[0]) < obj_rankify(b[0])

def compare_straights(a, b):
	return obj_rankify(a[0]) < obj_rankify(b[0])

def compare_threes_of_a_kind(a, b):
	return compare_ns_of_a_kind(a, b, 3)

def compare_two_pairses(a, b):
	a_n_of_a_kind_elems = get_n_of_a_kind_elems(a, 2)
	b_n_of_a_kind_elems = get_n_of_a_kind_elems(b, 2)

	a_n_of_a_kind_elems.sort(key=lambda x:obj_rankify(x))
	b_n_of_a_kind_elems.sort(key=lambda x:obj_rankify(x))

	if obj_rankify(a_n_of_a_kind_elems[0]) != obj_rankify(b_n_of_a_kind_elems[0]):
		return obj_rankify(a_n_of_a_kind_elems[0]) < obj_rankify(b_n_of_a_kind_elems[0])
	else:
		return obj_rankify(a_n_of_a_kind_elems[1]) < obj_rankify(b_n_of_a_kind_elems[1])


def compare_one_pairses(a, b):
	return compare_ns_of_a_kind(a, b, 2)

def compare_high_cards(a, b):
	return obj_rankify(a[0]) < obj_rankify(b[0])

def less_hand_ranks(a, b):
	if len(a[1]) != 5:
		pdb.set_trace()

	if a[0] < b[0]: return True
	if a[0] > b[0]: return False

	a = a[1]
	b = b[1]

	a.sort(key=lambda x:obj_rankify(x[RANK]))
	b.sort(key=lambda x:obj_rankify(x[RANK]))
	a.reverse()
	b.reverse()

	if is_royal_flush(a):       return compare_royal_flushes(a, b)
	elif is_straight_flush(a):  return compare_straight_flushes(a, b)
	elif is_four_of_a_kind(a):  return compare_fours_of_a_kind(a, b)
	elif is_full_house(a):      return compare_full_houses(a, b)
	elif is_flush(a):           return compare_flushes(a, b)
	elif is_straight(a):        return compare_straights(a, b)
	elif is_three_of_a_kind(a): return compare_threes_of_a_kind(a, b)
	elif is_two_pairs(a):       return compare_two_pairses(a, b)
	elif is_one_pair(a):        return compare_one_pairses(a, b)
	elif is_high_card(a):       return compare_high_cards(a, b)

	for i in xrange(len(a[1])):
		if obj_rankify(a[1][i]) < obj_rankify(b[1][i]): return True
		if obj_rankify(a[1][i]) > obj_rankify(b[1][i]): return False
	print 'tie: ', a, b
	return False

def calc_winner(a_hand, b_hand):
	if len(a_hand) != 5:
		pdb.set_trace()

	if less_hand_ranks(get_hand_rank(a_hand), get_hand_rank(b_hand)):
		return 0
	else:
		return 1

def calc_winner_print(a_hand, b_hand):
	x = None
	if less_hand_ranks(get_hand_rank(a_hand), get_hand_rank(b_hand)):
		x = 0
	else:
		x = 1

	a_hand.sort(key=lambda x:obj_rankify(x[RANK]))
	b_hand.sort(key=lambda x:obj_rankify(x[RANK]))
	print a_hand, ' - ', get_hand_rank_as_string(a_hand),
	if x == 0:
		print ''
	else:
		print ' #WINNER'
	print b_hand, ' - ', get_hand_rank_as_string(b_hand),
	if x == 1:
		print ''
		print ''
	else:
		print ' #WINNER'
		print ''

	return x

def read_in_hands():
	a_hands = []
	b_hands = []
	for line in open('poker.txt', 'r'):
		a_hands.append(line.split()[:5])
		b_hands.append(line.split()[5:])
	return (a_hands, b_hands)

(a_hands, b_hands) = read_in_hands()
a_wins = sum(euler.zip_with(a_hands, b_hands, calc_winner_print))

print a_wins - 1 # number 25. Don't judge.