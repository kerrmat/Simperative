# Simperative

## Team members
- Matt Kerr -> kerrmat@oregonstate.edu
- Lane Thompson -> thomlane@oregonstate.edu

## The Simperative Language
Simperative is an imperative language using Haskell as it's meta language. It takes a program and the environment and returns a modified environment.

## Program execution
Simperative is meant to be run on GHCi using the module ImperativeSyntax with only the prelude module loaded.

- Good examples - 

  After Loading module, type testTwenty - Runs a program that calls a function to adds two ints together, then adds one to that int until it reaches 20
  Expected Output: 
	Ints: (x,5), (y,5), (z,10), (Count,20),
	Bools:
	Strings:
	
  After Loading module, type testCardGood - Runs birthdayCard program, which generates a birthday card given a name and number of "very"s, on an environment that has a valid "name" and "v" variable
  Expected Output:
	Ints: (v,0),
	Bools: (f,True),
	Strings: (name,Matt), (card,Happy Birthday Matt,
	I hope you have a very very very good birthday),
  
  
- Bad examples - 

  After loading module, type testBadCall - Runs a program that calls a nonexistent function.
  Expected Output: 
	Error: Cannot reference a nonexistent function
  
  After loading module, type testCardBad - Runs birthdayCard program with an invalid environment, where not all required variables are declared
  Expected Output: 
	Error: Cannot reference a nonexistent variable
  
  
