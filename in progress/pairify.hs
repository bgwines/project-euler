pairify l = (a, b) : pairify l'
	where
		a = head l
		b = head . tail $ l
		l' = tail . tail $ l