find_running_bests_rec :: [([Integer], Integer)] -> ([Integer], Integer) -> [([Integer], Integer)]
find_running_bests_rec [] so_far = []
find_running_bests_rec l so_far = 
	case (snd so_far) `compare` (snd e) of
		LT -> e : (find_running_bests_rec (tail l) e)
		EQ -> e : (find_running_bests_rec (tail l) e)
		GT ->      find_running_bests_rec (tail l) so_far
	where e = head l

find_running_bests :: [([Integer], Integer)] -> [([Integer], Integer)]
find_running_bests [] = []
find_running_bests l = (head l) : find_running_bests_rec (tail l) (head l)
