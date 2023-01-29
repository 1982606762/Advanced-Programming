KUids:

kxs806
qmc887

# Design and implementation

## showExp

First I think of a way to show the pure number, for it's the base of the nexgt part. i found the function "show" quite useful in this part, so just show it. Then I found if the result is negative number I need to add parentheses to the both side of it. 

Then I add the rest of this function part.

## evalSimple

This function is a bit simple, because it returns an integer, so first I let it return the actual x. Then it needs to calculate {+,-,*,/}, just apply the built-in function to calculate them, and 'div' can do the floor thing, just need to check if the  dividend is zero and return an error. 

In power it just needs to check if y< 0 it would throw an error or it would be  calculated with origin power function.

## extendEnv 

In part 2 of the assignment there's a new thing called environment. In my opinion I think of it like a map structure. You input a name in it and get an integer output. With this recognition I did the rest of the assignment.

As the env is a "map", so extend env is just insert a new number into it. I made my function following the example given("ans == 42") and as it needs to save previous variables, if it didn't match then just find the variable in the previous environment.

## evalFull 

This function should use all thoughts used for previous questions. It receives an expression and an environment and return an integer , so the function should equal to the result Exp calculated in the env. 

Similarly I defined pure number first, as it doesn't an env to calculate so it just return pure value. Then there're {+,-,*,/,^}, all very similar as evalsimple . Then there's if. You need to first calculate the expression in "test" in the environment and check if it equals to 0,then just calculate the rest part. 

About the var ,just use environment to search for var. 

About Let, I tried to first calculate the eval of "def" and then save it in a temp env with the name of var. Then just calculate the body in the temp env.

About Sum,I first calculate the eval of evalfrom and evalto and create a new env to save the evalfrom. Then use list comprehension to get x from evalfrom to evalto. then I save the x in a new env and calc the body in new env.

If there's unneed part in Let I just continue the calculation with new envirment with unneed part in it, because it's simple to imply.

## evalErr

This need to use either and Left stands for error, Right stands for result.

Also if a pure number is passed in, I return an right number, in other case the input could possibly be Right, so I use tuple to match two possible situation. If both are Right then return Right answer, then there's a Left in the tuple, just return it.

For if Let and Sum just evaluate the part which could possibly have an error with evalErr and then return Right answer(using previous evalFull code) or Left error.

In this function I also don't throw error if there's unused part in Let expression, because it's also simple to imply.

# assessment

## Completeness

I had implemented all necessary part of the assignment and didn't do the optional part of coding.

## Correctness

I did test during developing in ghci and because of unknown reason I can't use stack test. My code passed all the tests in online TA so I think the correctness is pretty high.

## Efficiency

I didn't use time or space function, but I think the code works just fine for there's only necessary processure in the code.

## Maintainability

I used the name of variables given in the question and also used as less length of each line as possible, that will make the code much more easy to read and fix in the future.

## Other

There are two warnings if use ghci -W to compile the code file. They are because I tried to throw an error when try to eval expressions that not included in the question, but I don't know why the complier say they are unused.....
