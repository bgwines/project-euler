
import List

as = [2..100]
bs = [2..100]

powers = [a^b | a<-as, b<-bs]

s = fill_set powers

sought = Set.size s