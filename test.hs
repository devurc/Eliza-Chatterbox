import System.Random
import Chatterbot

main = 
    putStrLn "hello world!\n"
    

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = (if x > 100
                        then x
                        else x*2) + 1

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG" | x <- xs, odd x ]

length' xs = sum [1 | _ <- xs ]

lengthRecurse :: (Num b) => [a] -> b
lengthRecurse [] = 0
-- how does lengthRecurse xs become the tail of the original xs?
lengthRecurse (_:xs) = 1 + lengthRecurse xs

fact :: (Integral a) => a -> a
fact 0 = 1
fact n = n * fact (n - 1)

head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x

last' :: [a] -> a
last' [] = error "empty list"
-- has incorrect output for list of size 1, so adding single element list case
last' (x:[]) = x
last' (x:xs) = xs !! (length' xs - 1)

init' :: [a] -> [a]
init' [] = error "empty list"
-- init' (x:xs) = [ x | x <- xs, ]

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b


rollDice :: IO ()
rollDice = do
    r <- randomIO :: IO Float
    putStrLn ("You rolled " ++ show (floor (6*r+1)))
    putStrLn ("r is " ++ show(r))


singleExample = [
  ("I need *",
      ["Why do you need * ?"])
  ]

multiExample = [
  ("I need *",
      ["Why do you need * ?",
       "Would it really help you to get * ?",
       "Are you sure you need * ?"])
  ]

realExample = [
  ("I need *",
      ["Why do you need * ?",
       "Would it really help you to get * ?",
       "Are you sure you need * ?"]),

  ("Why don't you *",
      ["Do you really think I don't * ?",
       "Perhaps eventually I will * .",
       "Do you really want me to * ?"])
  ]

rulesCompile2 :: [(String, [String])] -> BotBrain
rulesCompile2 [] = []
rulesCompile2 (x:xs) =
  ( words $ fst x, [ words $ y | y<-(snd x) ] ) : rulesCompile xs

fstExtract :: [(String, [String])] -> Phrase
fstExtract (x:xs) = 
  words $ fst x

sndExtract :: [(String, [String])] -> [Phrase]
sndExtract (x:xs) = 
  [ words $ y | y<-(snd x) ]

multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z  

mult9 :: (Num a) => a -> a -> a
mult9 = multThree 9

eliza = [
  ("",
      ["Speak up! I can't hear you."]),

  ("I need *",
      ["Why do you need * ?",
       "Would it really help you to get * ?",
       "Are you sure you need * ?"]),

  ("Why don't you *",
      ["Do you really think I don't * ?",
       "Perhaps eventually I will * .",
       "Do you really want me to * ?"]),

  ("Why can't I *",
      ["Do you think you should be able to * ?",
       "If you could * , what would you do?",
       "I don't know -- why can't you * ?",
       "Have you really tried?"]),

  ("I can't *",
      ["How do you know you can't * ?",
       "Perhaps you could * if you tried.",
       "What would it take for you to * ?"]),

  ("I am *",
      ["Did you come to me because you are * ?",
       "How long have you been * ?",
       "How do you feel about being * ?"]),

  ("I'm *",
      ["How does being * make you feel?",
       "Do you enjoy being * ?",
       "Why do you tell me you're * ?",
       "Why do you think you're * ?"]),

  ("Are you *",
      ["Why does it matter whether I am * ?",
       "Would you prefer it if I were not * ?",
       "Perhaps you believe I am * .",
       "I may be * -- what do you think?"]),

  ("What is Haskell",
      ["Oh, I thought you knew that. It is a functional " ++
       "programming language featuring strong typing and " ++
       "lazy evaluation."]),

  ("What *",
      ["Why do you ask?",
       "How would an answer to that help you?",
       "What do you think?"]),

  ("How *",
      ["How do you suppose?",
       "Perhaps you can answer your own question.",
       "What is it you're really asking?"]),

  ("Because *",
      ["Is that the real reason?",
       "What other reasons come to mind?",
       "Does that reason apply to anything else?",
       "If * , what else must be true?"]),

  ("* sorry *",
      ["There are many times when no apology is needed.",
       "What feelings do you have when you apologize?"]),

  ("Hello *",
      ["Hello... I'm glad you could drop by today.",
       "Hi there... how are you today?",
       "Hello, how are you feeling today?"]),

  ("I think *",
      ["Do you doubt * ?",
       "Do you really think so?",
       "But you're not sure * ?"]),

  ("* friend *",
      ["Tell me more about your friends.",
       "When you think of a friend, what comes to mind?",
       "Why don't you tell me about a childhood friend?"]),

  ("Yes",
      ["You seem quite sure.",
       "OK, but can you elaborate a bit?"]),

  ("* computer *",
      ["Are you really talking about me?",
       "Does it seem strange to talk to a computer?",
       "How do computers make you feel?",
       "Do you feel threatened by computers?"]),

  ("Is it *",
      ["Do you think it is * ?",
       "Perhaps it's * -- what do you think?",
       "If it were * , what would you do?",
       "It could well be that * ."]),

  ("It is *",
      ["You seem very certain.",
       "If I told you that it probably isn't * , what would you feel?"]),

  ("Can you *",
      ["What makes you think I can't * ?",
       "If I could * , then what?",
       "Why do you ask if I can * ?"]),

  ("Can I *",
      ["Perhaps you don't want to * .",
       "Do you want to be able to * ?",
       "If you could * , would you?"]),

  ("You are *",
      ["Why do you think I am * ?",
       "Does it please you to think that I'm * ?",
       "Perhaps you would like me to be * .",
       "Perhaps you're really talking about yourself?"]),

  ("You're *",
      ["Why do you say I am * ?",
       "Why do you think I am * ?",
       "Are we talking about you, or me?"]),

  ("I don't *",
      ["Don't you really * ?",
       "Why don't you * ?",
       "Do you want to * ?"]),

  ("I feel *",
      ["Good, tell me more about these feelings.",
       "Do you often feel * ?",
       "When do you usually feel * ?",
       "When you feel * , what do you do?"]),

  ("I have *",
      ["Why do you tell me that you've * ?",
       "Have you really * ?",
       "Now that you have * , what will you do next?"]),

  ("I would *",
      ["Could you explain why you would * ?",
       "Why would you * ?",
       "Who else knows that you would * ?"]),

  ("Is there *",
      ["Do you think there is * ?",
       "It's likely that there is * .",
       "Would you like there to be * ?"]),

  ("My *",
      ["I see, your * .",
       "Why do you say that your * ?",
       "When your * , how do you feel?"]),

  ("You *",
      ["We should be discussing you, not me.",
       "Why do you say that about me?",
       "Why do you care whether I * ?"]),

  ("Why *",
      ["Why don't you tell me the reason why * ?",
       "Why do you think * ?" ]),

  ("I want *",
      ["What would it mean to you if you got * ?",
       "Why do you want * ?",
       "What would you do if you got * ?",
       "If you got * , then what would you do?"]),

  ("* mother *",
      ["Tell me more about your mother.",
       "What was your relationship with your mother like?",
       "How do you feel about your mother?",
       "How does this relate to your feelings today?",
       "Good family relations are important."]),

  ("* father *",
      ["Tell me more about your father.",
       "How did your father make you feel?",
       "How do you feel about your father?",
       "Does your relationship with your father relate to your feelings today?",
       "Do you have trouble showing affection with your family?"]),

  ("* child *",
      ["Did you have close friends as a child?",
       "What is your favorite childhood memory?",
       "Do you remember any dreams or nightmares from childhood?",
       "Did the other children sometimes tease you?",
       "How do you think your childhood experiences relate to your feelings today?"]),

  ("* ?",
      ["Why do you ask that?",
       "Please consider whether you can answer your own question.",
       "Perhaps the answer lies within yourself?",
       "Why don't you tell me?"]),

  ("quit",
      ["Thank you for talking with me.",
       "Good-bye.",
       "Thank you, that will be $150.  Have a good day!"]),

  ("*",
      ["Please tell me more.",
       "Let's change focus a bit... Tell me about your family.",
       "Can you elaborate on that?",
       "Why do you say that * ?",
       "I see.",
       "Very interesting.",
       "* .",
       "I see.  And what does that tell you?",
       "How does that make you feel?",
       "How do you feel when you say that?"])
  ]


 
