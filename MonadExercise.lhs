> {-# LANGUAGE CPP #-}
> {-# LANGUAGE Safe #-}
> module MonadExercise where
>
> import Control.Monad (liftM, ap)
> import Control.Applicative (Applicative(..))

Assessed monad exercises
------------------------

There are four exercises, 25% each. Replace each occurrence of
"undefined" by your own code.

Submissions that don't compile won't be accepted. If you attempted an
exercise but didn't succeed, make sure you remove the code, and
replace it back to undefined, before you submit.

The formulation of the exercises is perhaps long, but their solution
requires very little coding, compared to the amount of thinking. Read
carefully.

Out of the four exercises, one of them is perhaps hard,
deliberately. The other ones amount to just understanding what we did
in the lectures and corresponding handouts.

Three exercises ask you to use monads, and one to complete the
definition of a monad.

You will not get a testbench this time, deliberately, for learning
purposes. However, I will suggest some tests for you to perform, so
that you can gain some confidence that your answers are correct.

In order to attempt this, you must be familiar with:

  * The lecture contents (on Canvas), in particular

        lecture7-handout.lhs
        lecture11-handout.lhs
    
    but also everything else we did.

    NB. The lecture number refers to the first lecture the handout was
    used. Typically, the same handout is used/discussed/taught in 
    several lectures.

  * The mandatory, unassessed monad exercise 
    (also on Canvas, with sample solutions).

  * Our textbook http://learnyouahaskell.com/chapters
    (mandatory, unassessed reading).

Questions
----------

Mathematicians don't know everything. Even rather simple things such
as the following are not known.

 (1) Start with an integer n>1.
 (2) If it is even, divide it by two.
 (3) If it is odd, multiply it by three and add 1.
 (4) Then carry on, either forever, or until you reach 1.

Example. Starting with n=6, we get 6, 3, 10, 5, 16, 8, 4, 2, 1.

Conjecture. We will always reach 1, eventually, no matter
what the starting number n is.
https://en.wikipedia.org/wiki/Collatz_conjecture

But nobody really knows, at present. (Will we ever know for sure?)

Mathematicians have checked all numbers n<=2^60, which is quite large,
even for computers, and showed that you always eventually reach 1 (by
indirect, mathematical means that avoid doing all the calculations
explicitly).

Don't worry, the exercise is not to answer this mathematical question.
We are helping a mathematician, who wants to see experimental results.

I write a function 

> collatz :: Integer -> ()
> collatz n | n<1    = error "only numbers >=1 qualify"
>           | n == 1 = ()                 -- We end when n==1.
>           | even n = collatz(n `div` 2) -- We loop.
>           | odd  n = collatz(3 * n + 1) -- We loop.

Now we run 

*> collatz 6
()

This is not very exciting: we know that the process starting with 6
terminates, with the uninteresting value (), but we don't get to see
the trajectory as in the above example.

So we will use the Writer monad defined in lecture11-handout.lhs
available from Canvas.

> data Writer a = Result a String deriving Show

> instance Functor Writer where
>   fmap  = liftM
>
> instance Applicative Writer where
>   pure = return
>   (<*>) = ap
>
> instance Monad Writer where
>   return x = Result x ""
>   xm >>= f = case xm of
>                Result x s -> case f x of
>                                Result y t -> Result y (s ++ t)

> tell :: String -> Writer ()
> tell s = Result () s

Now look at fib and wfib in lecture11-handout.lhs. I repeat wfib here,
but do read that handout paying attention to the details.

> wfib :: Integer -> Writer Integer
> wfib n = do
>   tell (show n ++ ", ")
>   if n == 0 || n == 1
>          then return n
>          else do x <- wfib(n-1) 
>                  y <- wfib(n-2)
>                  return(x + y)

Example (try it youself):

*> wfib 5
Result 5 "5, 4, 3, 2, 1, 0, 1, 2, 1, 0, 3, 2, 1, 0, 1, "

Exercise (25%). 
---------------
Define the following function so that the trajectory is produced:

> wcollatz :: Integer -> Writer ()
> wcollatz n | n<1    = error "only numbers >=1 qualify"
>            | n == 1 = do
>                            return ()
>            | even n = do
>                            tell (show n++", ")
>                            e <- (wcollatz (n `div` 2))

>                            return e
>            | odd  n = do
>                            tell (show n++", ")
>                            o <- (wcollatz ((3*n)+1)) 
>                            return o

If you got this right, you should get, for instance:

*> wcollatz 6
Result () "6, 3, 10, 5, 16, 8, 4, 2, "

Exercise (25%). 
---------------
We instead want to use the counter monad of lecture11-handout.lhs so
that we count how many odd numbers occur in the trajectory. Now you
should cheat from cfib in that handout.

> data Counter a = Count a Int deriving Show

> instance Functor Counter where
>   fmap  = liftM
>
> instance Applicative Counter where
>   pure = return
>   (<*>) = ap
>
> instance Monad Counter where
>  return x = Count x 0
>  xm >>= f = case xm of
>               Count x c -> case f x of
>                              Count y d -> Count y (c + d)

> count :: Counter ()
> count = Count () 1

> oddcollatz :: Integer -> Counter ()
> oddcollatz n | n<1    = error "only numbers >=1 qualify"
>              | n == 1 = do
>                              return ()
>              | even n = do
>                              e <- (oddcollatz (n `div` 2))
>                              return e
>              | odd  n = do
>                              Count () 1
>                              o <- (oddcollatz ((n*3)+1))
>                              return o


You should get, for instance,

*> oddcollatz 6
Count () 2

With the above you can pass this exercise. Now let's try slightly more
challenging things.

Exercise (25%). 
---------------
Instead use the state monad to achieve the same effect of counting
odds, where the counter is the state.

> data State s a = T(s -> (a,s)) 

> runState :: State s a -> (s -> (a, s))
> runState (T p) = p

> instance Functor (State s) where
>   fmap  = liftM
>
> instance Applicative (State s) where
>   pure = return
>   (<*>) = ap
>
> instance Monad (State s) where
>  return x = T(\u -> (x,u))
>  xm >>= f = case xm of
>               T p -> T(\u -> case p u of
>                                (x, v) -> case f x of 
>                                            T q -> q v)

> get :: State s s
> get = T(\s -> (s,s))

> put :: s -> State s ()
> put s = T (\_ -> ((), s))

> modify :: (s -> s) -> State s ()
> modify f = T(\s -> ((), f s))

Example:

> sfib :: Integer -> (Integer , Integer)
> sfib n = runState (f n) 0
>   where f n = do
>         modify succ
>         if n == 0 || n == 1
>          then return n
>          else do x <- f(n-1)
>                  y <- f(n-2)
>                  return(x + y)

Here is your exercise: Count how many odd numbers occur in the
trajectory.

> soddcollatz :: Integer -> Integer
> soddcollatz n = snd(runState (f n) 0)
>   where f n  | n<1    = error "only numbers >=1 qualify"          
>              | n == 1 = return ()
>              | even n = do
>                             e <- (f(n `div` 2))
>                             return e
>           
>              | odd  n = do
>                             modify succ
>                             o <- (f((n*3)+1))
>                             return o

What is "snd"? Find out.

Exercise (25%). 
---------------
Define your own monad. Now this is harder, be warned.

If we have a recursively defined function, it may never terminate. We
may want to timeout after too many recursive calls are performed.

For this purpose, I ask you to define a timeout monad. 

> data Timeout a = Tick (Int -> Maybe (a , Int))
> runTimeout :: Timeout a -> Int -> Maybe (a, Int)
> runTimeout (Tick x) = x

> instance Functor Timeout where
>   fmap  = liftM
>
> instance Applicative Timeout where
>   pure = return
>   (<*>) = ap

> instance Monad Timeout where
>  return a = Tick (\x -> Just (a, x))                     
>  ma >>= f = case ma of
>             Tick p -> Tick(\u -> case p u of
>                                       Nothing -> Nothing
>                                       Just(x, v) -> case f x of
>                                                        Tick q -> q v)


> tick :: Timeout ()
> tick = Tick f
>  where
>    f n | n <= 0    = Nothing  
>        | otherwise = Just ((), n - 1)

Given

> tfib :: Integer -> Timeout Integer
> tfib 0 = return 1
> tfib 1 = return 1
> tfib n = do
>   tick
>   x <- tfib (n - 1)
>   y <- tfib (n - 2)
>   return (x + y)

you are supposed to get, for instance:

*> runTimeout (tfib 5) 6
Nothing

This means that 6 recursive calls are not enough to get an answer.

*> runTimeout (tfib 5) 7
Just (8,0)

This means that the answer is 8, and your bank account for recursive
calls is empty.

*> runTimeout (tfib 5) 8
Just (8,1)

Again the result is eight, and you didn't spend all your money to
compute the 5th fibonacci number. You have one tick left, which you
may use for other purposes.

And so on:

*> runTimeout (tfib 5) 9
Just (8,2)
*> runTimeout (tfib 5) 20
Just (8,13)
*> runTimeout (tfib 5) 10
Just (8,3)

Your task is to complete the above "undefined"s so that you achieve
this.
