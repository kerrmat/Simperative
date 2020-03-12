# Simperative

## Team members
- Matt Kerr -> kerrmat@oregonstate.edu
- Lane Thompson -> thomlane@oregonstate.edu

## The Simperative Language
Simperative is an imperative language using Haskell as it's meta language. It takes a program and the environment and returns a modified environment.

## Program execution
Simperative is meant to be run on GHCi with the module ImperativeSyntax loaded.

- Static Typing -

	To run our static type checker on each of our example programs, type the following commands:
	
	**typeOfProgram birthdayCard**
	
	**typeOfProgram passwordProgram**
	
	**typeOfProgram birthdayCardBad**
	
	**typeOfProgram badFC**
	\n

	The expected outcome on all of these commands should be True, since they all have valid typing.

- Good examples - 

  After Loading module, type **testPassword** - Runs passwordProgram program that checks if there is a string already in the environment that matches the string "password". Pushes "correct password" message to env
  Expected Output: 
	Ints:
	Bools:
	Strings: (password, password), (message, Correct Password, Congratulations)
	
  After Loading module, type **testCardGood** - Runs birthdayCard program, which generates a birthday card given a name and number of "very"s, on an environment that has a valid "name" and "v" variable
  Expected Output:
	Ints: (v,0),
	Bools: (f,True),
	Strings: (name,Matt), (card,Happy Birthday Matt,
	I hope you have a very very very good birthday),
  
  
- Bad examples - 

  After Loading module, type **testWrongPassword** - Runs passwordProgram program that checks if there is a string already in the environment that matches the string "password". Pushes "incorrect password" message to env
  Expected Output: 
	Ints:
	Bools:
	Strings: (password, salami), (message, Incorrect Password, Sorry)

  After loading module, type **testBadCall** - Runs a program that calls a nonexistent function.
  Expected Output: 
	Error: Cannot reference a nonexistent function
  
  After loading module, type **testCardBad** - Runs birthdayCardBad program with an invalid environment, where not all required variables are declared
  Expected Output: 
	Error: Cannot reference a nonexistent variable
  
  
