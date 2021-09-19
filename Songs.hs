module Songs where
import Data.Tuple.Extra
import General


arp :: [ScaleDegree] -> Sequence
arp = map (\x -> ([x], 1))

sqRest :: ([ScaleDegree], Seconds)
sqRest = ([], 1)

qRest :: ([ScaleDegree], Seconds)
qRest = ([], 2)

cRest :: ([ScaleDegree], Seconds)
cRest = ([], 4)

loop :: Int -> [a] -> [a]
loop 0 _ = []
loop 1 xs = xs
loop n xs = xs ++ loop (n-1) xs

transposeSequence :: ScaleDegree -> Sequence -> Sequence
transposeSequence offset = map $ first $ map (+offset)



sailorsHornpipe :: [((Maybe ScaleDegree), Seconds)]
sailorsHornpipe = map (\(x,y) -> (fmap (\z -> z-1) x,y)) $ [(Just 8, 1), (Just 7, 1), 
                                            (Just 8, 1), (Nothing, 1), (Just 1, 1), (Nothing, 1), (Just 1, 2), (Just 5, 1), (Just 4, 1), 
                                            (Just 3, 1), (Just 5, 1), (Just 8, 1), (Nothing, 1), (Just 8, 1), (Just 10, 1), (Just 9, 1), (Just 8, 1), 
                                            (Just 9, 1), (Nothing, 1), (Just 2, 1), (Nothing, 1), (Just 2, 1), (Just 9, 1), (Just 9, 1), (Just 8, 1), 
                                            (Just 7, 2), (Just 5, 1), (Nothing, 1), (Just 5, 2)] ++ semis [6, 7, 
                                            8, 7, 6, 5, 6, 5, 4, 3, 
                                            4, 3, 2, 1, 2, 1, 0, -2,
                                            -1, 1, 0, 2, 1, 3, 2, 4, 
                                            3] ++ [(Nothing, 1), (Just 1, 1), (Nothing, 1), (Just 1, 1), (Nothing, 1)]
    where 
        -- just make a list of notes with given pitches and duration of 1
        semis = map (\n -> (Just n, 1))



silentNightMelody :: Sequence
silentNightMelody = transposeSequence 7 [([4], 3), ([5], 1), ([4], 2), ([2], 5), sqRest,
                ([4], 3), ([5], 1), ([4], 2), ([2], 5), sqRest,
                ([8], 4), ([8], 2), ([6], 4), qRest,
                ([7], 4), ([7], 2), ([4], 4), qRest, 
                ([5], 4), ([5], 2), ([7], 3), ([6], 1), ([5], 2), 
                ([4], 3), ([5], 1), ([4], 2), ([2], 5), sqRest,
                ([5], 4), ([5], 2), ([7], 3), ([6], 1), ([5], 2), 
                ([4], 3), ([5], 1), ([4], 2), ([2], 5), sqRest,
                ([8], 4), ([8], 2), ([10], 3), ([8], 1), ([6], 2),
                ([7], 6), ([9], 6),
                ([7], 2), ([4], 2), ([2], 2), ([4], 3), ([3], 1), ([1], 2),
                ([0], 6), ([], 6)]

silentNightChords :: Sequence
silentNightChords = [([2, 4], 3), ([3, 5], 1), ([2, 4], 2), ([0, 2], 5), sqRest,
                ([2, 4], 3), ([3, 5], 1), ([2, 4], 2), ([0, 2], 5), sqRest,
                ([3, 6, 8], 4), ([3, 6, 8], 2), ([3, 4, 6], 4), qRest,
                ([2, 7], 4), ([2, 7], 2), ([2, 4], 4), qRest, 
                ([3, 5], 4), ([3, 5], 2), ([5, 7], 3), ([4, 6], 1), ([3, 5], 2), 
                ([2, 4], 3), ([3, 5], 1), ([2, 4], 2), ([0, 2], 5), sqRest,
                ([3, 5], 4), ([3, 5], 2), ([5, 7], 3), ([4, 6], 1), ([3, 5], 2), 
                ([2, 4], 3), ([3, 5], 1), ([2, 4], 2), ([0, 2], 3), ([2], 1), ([4], 1), ([7], 1),
                ([3, 6, 8], 4), ([3, 6, 8], 2), ([6, 8, 10], 3), ([3, 6, 8], 1), ([3, 6], 2),
                ([2, 7], 4), ([4], 1), ([7], 1),   ([4, 7, 9], 3), ([0], 1), ([2], 1), ([4], 1),
                ([2, 7], 2), ([2, 4], 2), ([0, 2], 2), ([-1, 4], 3), ([3], 1), ([-1, 1], 2),
                ([0], 4), ([2], 1), ([4], 1), ([0, 2, 4, 7], 6)]


silentNightBassline :: Sequence
silentNightBassline = transposeSequence (-14) $ tonicArp ++ tonicArp ++ --silent night
                                            tonicArp ++ tonicArp ++ --holy night
                                            fifthArp ++ fifthArp ++ --all is calm
                                            tonicArp ++ tonicArp ++ --all is bright
                                            fourthArp ++ fourthArp ++ --round young virgin
                                            tonicArp ++ tonicArp ++ --mother and child
                                            fourthArp ++ fourthArp ++ --holy infant so
                                            tonicArp ++ tonicArp ++ --tender and mild
                                            fifthArp ++ fifthArp ++ --sleep in heavenly
                                            octaveArp ++ [([0], 1), ([4], 1), ([7], 4)] ++ --peace
                                            tonicArp ++ [([4], 1), ([8], 1), ([13], 2), ([4], 2)] ++ --sleep in heavenly
                                            [([0], 1), ([2], 1), ([4], 1), ([4,7], 3)] ++ --peace 
                                            [([0, 4, 7], 6)]
                                           
    where
        tonicArp = [([0], 1), ([4], 1), ([9], 2), ([7], 2)]
        octaveArp = [([7], 1), ([11], 1), ([14], 1), ([11], 1)]
        fifthArp = [([4], 1), ([8], 1), ([13], 2), ([8], 2)]
        fourthArp = [([3], 1), ([7], 1), ([12], 2), ([7], 2)]



jump :: [([ScaleDegree], Seconds)]
jump = (loop 3 intro) ++ intro'
    where
        intro = [cRest,    ([4, 6, 8], 2), qRest,    qRest, ([4, 7, 9], 2),        ([], 4), 
                ([3, 5, 7], 2), qRest,     qRest, ([3, 5, 7], 2),   qRest, ([4, 6, 8], 1), sqRest,     qRest, ([4, 6, 8], 5), sqRest,
                    ([4, 7, 9], 2), qRest,     qRest, ([3, 5, 7], 1), sqRest,     qRest, ([0, 3, 5], 4),  
                ([0, 2, 4], 4),   ([0, 1, 4], 10)   ]
        intro' = [cRest,    ([10, 12, 14], 2), qRest,    qRest, ([10, 12, 14], 2),        ([], 4), 
                ([10, 12, 14], 2), qRest,     qRest, ([10, 12, 14], 2),   qRest, qRest,     ([7, 9, 11], 4), 
                    cRest,     ([3, 5, 7], 2), qRest, qRest,  ([0, 3, 5], 2), qRest, ([0, 3, 5], 4),  
                ([0, 2, 4], 4),   ([0, 1, 4], 10)   ]



jumpBassline :: [([ScaleDegree], Seconds)]
jumpBassline = transposeSequence (-21) $ (loop 2 intro) ++ (loop 2 verse)
    where
        intro = [([0, 7], 50), ([3, 10], 4), ([4, 11], 4), ([11], 2), ([10], 2), ([11], 2)]
        verse = (loop 25 [([0, 7], 1), ([], 1)]) ++ [([3, 10], 4), ([4, 11], 4), ([11], 2), ([10], 2), ([11], 2)]







