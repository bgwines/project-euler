
main :: IO ()
main = do
	putStrLn . show $ sum . map (\n -> n^n) $ [1..1000]