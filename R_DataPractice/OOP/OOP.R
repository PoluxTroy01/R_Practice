# Object Oriented Programming
# 20 types of objects available in R
# 2 types for object oriented programming: list & environment
# When is OOP a good idea?
# It best works when you have a limited number of objects you completely understand the
# behavior of. For APIs because there is a limited number of responses that the website
# can provide and define objects to restore this responses, also in graphical user interfaces
# GUIs.
# Good for building tools for data analysis
# Good when you have a limited number of complex objects

# function overloading: functions split split into generic + method
# methods named: generic.class_of_the_input


# Creating a generic function 
# inlcude ... argument in case arguments need to be passed from one method to another
# Create get_n_elements
get_n_elements <- function(x, ...){
  UseMethod("get_n_elements")
}


# ||| Creating an S3 method (1) |||
# 1. The name of the method must be of the form:  generic.class
# 2. The method signature - that is, the arguments that are passed in to the method - must 
# contain the signature of the generic.

# Create a data.frame method for get_n_elements
get_n_elements.data.frame <- function(x, ...){
  return(nrow(x) * ncol(x))
}


# ||| Creating an S3 method (2) |||
# Rather than having to write dozens of methods for every kind of input, you can create a 
# method that handles all types that don't have a specific method. This is called the default 
# method; it always has the name generic.default. For example, print.default() will print any 
# type of object that doesn't have its own print() method.

# View pre-defined objects
ls.str()

# Create a default method for get_n_elements
get_n_elements.default <- function(x, ...){
  return(length(unlist(x)))
}

# Call the method on the ability.cov dataset
n_elements_ability.cov <- get_n_elements.default(ability.cov)



# methods() finds methods for a generic or a class
# .S3methods() only s3 methods

# example for generic:
  methods(print)
# example fot class:
  methods(class = data.frame)
  
  
  

  
# ||| Working with R6 |||
# The first step is to create a class generator for each of your objects 
# A class generator is a template that describes what data can be stored in the object
# and what functions can be aplied to the object.
# Class generator a.k.a factories
# Summary:
# Load the R6 package 
# Define class generators with R6Class()
# Class names should be UpperCamelCase
# Data fields stored in privte list
library(R6)

# Specifying the Microwave Oven Class
# Define microwave_oven_factory
microwave_oven_factory <- R6Class(
  "MicrowaveOven",
  private = list(
    power_rating_watts = 800
  )
)


# Making Microwave Ovens
microwave_oven <- microwave_oven_factory$new()


# Hiding Complexity with Encapsulation
# In OOP the term for separating the implementation of the object from its user interface
# is called encapsulation.
# The data fields in the private elements can be accesed like this: private$ 
# And the public elements can be accesed with: self$
# Store data in private list
# Store methods in public list


# Add a cook method to the factory definition
microwave_oven_factory <- R6Class(
  "MicrowaveOven",
  private = list(
    power_rating_watts = 800
  ),
  public = list(
    cook = function(time_seconds){
      Sys.sleep(time_seconds)
      print("Your food is cooked!")
    }
  )
)

# Create microwave oven object
a_microwave_oven = microwave_oven_factory$new()

# Call cook method for 1 second
a_microwave_oven$cook(1)






# ||| Close the door |||
# Add a close_door() method
microwave_oven_factory <- R6Class(
  "MicrowaveOven",
  private = list(
    power_rating_watts = 800,
    door_is_open = FALSE
  ),
  public = list(
    cook = function(time_seconds) {
      Sys.sleep(time_seconds)
      print("Your food is cooked!")
    },
    open_door = function() {
      private$door_is_open = TRUE
    },
    close_door = function() {
      private$door_is_open = FALSE
    }
  )
)


# ||| Initialize |||
# initialize() lets you set the values of the private fields when you create an R6 object

# Add an initialize method
microwave_oven_factory <- R6Class(
  "MicrowaveOven",
  private = list(
    power_rating_watts = 800,
    door_is_open = FALSE
  ),
  public = list(
    cook = function(time_seconds) {
      Sys.sleep(time_seconds)
      print("Your food is cooked!")
    },
    open_door = function() {
      private$door_is_open = TRUE
    },
    close_door = function() {
      private$door_is_open = FALSE
    },
    # Add initialize() method here
    initialize = function(power_rating_watts, door_is_open) {
      if(!missing(power_rating_watts))
      {
        private$power_rating_watts <- power_rating_watts
      }
      if(!missing(door_is_open))
      {
        private$door_is_open <- door_is_open
      }
    }
  )
)

# Make a microwave
a_microwave_oven <- microwave_oven_factory$new(
  power_rating_watts = 650,
  door_is_open = TRUE
)




# ||| Acces private data |||
# Add a binding for power rating
microwave_oven_factory <- R6Class(
  "MicrowaveOven",
  private = list(
    ..power_rating_watts = 800
  ),
  active = list(
    # add the binding here
    power_rating_watts = function(){
      private$..power_rating_watts
    }
    
  )
)

# Make a microwave 
a_microwave_oven <- microwave_oven_factory$new()

# Get the power rating
a_microwave_oven$power_rating_watts




# ||| Create a read/write binding |||
# Add a binding for power rating

microwave_oven_factory <- R6Class(
  "MicrowaveOven",
  private = list(
    ..power_rating_watts = 800,
    ..power_level_watts = 800
  ),
  # Add active list containing an active binding
  active = list(
    power_level_watts = function(value) {
      if(missing(value)) {
        private$..power_level_watts
      } else {
        assert_is_a_number(value)
        assert_all_are_in_closed_range(
          value, lower = 0, upper = private$..power_rating_watts
        )
        private$..power_level_watts <- value
      }
    }
  )
)

# Make a microwave 
a_microwave_oven <- microwave_oven_factory$new()

# Get the power level
a_microwave_oven$power_level_watts

# Try to set the power level to "400"
a_microwave_oven$power_level_watts <- "400"

# Try to set the power level to 1600 watts
a_microwave_oven$power_level_watts <- 1600

# Set the power level to 400 watts
a_microwave_oven$power_level_watts <- 400








# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- 

microwave_oven_factory <- R6Class(
  "MicrowaveOven",
  private = list(
    power_rating_watts = 800,
    door_is_open = FALSE
  ),
  public = list(
    cook = function(time_seconds) {
      Sys.sleep(time_seconds)
      print("Your food is cooked!")
    },
    open_door = function() {
      private$door_is_open = TRUE
    },
    close_door = function() {
      private$door_is_open = FALSE
    },
    # Add initialize() method here
    initialize = function(power_rating_watts, door_is_open) {
      if(!missing(power_rating_watts))
      {
        private$power_rating_watts <- power_rating_watts
      }
      if(!missing(door_is_open))
      {
        private$door_is_open <- door_is_open
      }
    }
  ),
  active = list(
    power_level_watts = function(value) {
      if(missing(value)) {
        private$..power_level_watts
      } else {
        assert_is_a_number(value)
        assert_all_are_in_closed_range(
          value, lower = 0, upper = private$..power_rating_watts
        )
        private$..power_level_watts <- value
      }
    }
  )
)

# <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- <-









                          # ||| R6 inheritance |||

# ||| The pattern to create a child class |||
child_class_factory <- R6Class(
  "ChildClass",
  inherit = parent_class_factory
)


# Explore the microwave oven class
microwave_oven_factory

# Define a fancy microwave class inheriting from microwave oven
fancy_microwave_oven_factory <- R6Class(
  "FancyMicrowaveOven",
  inherit = microwave_oven_factory
)




# Inheritance means that the methods of the child class are exact copies of those in the 
# parent class.




# Explore microwave oven classes
microwave_oven_factory
fancy_microwave_oven_factory

# Instantiate both types of microwave
a_microwave_oven <- microwave_oven_factory$new()
a_fancy_microwave <- fancy_microwave_oven_factory$new()

# Get power rating for each microwave
microwave_power_rating <- a_microwave_oven$power_rating_watts
fancy_microwave_power_rating <- a_fancy_microwave$power_rating_watts

# Verify that these are the same
identical(microwave_power_rating, fancy_microwave_power_rating)

# Cook with each microwave (1 second)
a_microwave_oven$cook(1)
a_fancy_microwave$cook(1)






# private$ accesses private fields
# self$ accesses public methods in self
# super$ accesses public methods in parents





# ||| Extending the cooking capabilities |||
# Explore microwave oven class
microwave_oven_factory

# Extend the class definition
fancy_microwave_oven_factory <- R6Class(
  "FancyMicrowaveOven",
  inherit = microwave_oven_factory,
  # Add a public list with a cook baked potato method
  public = list(
    cook_baked_potato = function(){
      self$cook(3)
    }
  )
  
)

# Instantiate a fancy microwave
a_fancy_microwave <- fancy_microwave_oven_factory$new() 

# Call the cook_baked_potato() method
a_fancy_microwave$cook_baked_potato()







# ||| Overriding the cooking capabilities |||
# Child classes can also extend functionality by overriding methods. They do this by 
# defining methods with the same name as that of the parent.
# 
# Child classes can access public methods from their parent class by prefixing the name 
# with super$.

# Explore microwave oven class
microwave_oven_factory

# Update the class definition
fancy_microwave_oven_factory <- R6Class(
  "FancyMicrowaveOven",
  inherit = microwave_oven_factory,
  # Add a public list with a cook method
  public = list(
    cook = function(time_seconds){
      super$cook(time_seconds)
      message("Enjoy your dinner!")
    }
  )
  
)

# Instantiate a fancy microwave
a_fancy_microwave <- fancy_microwave_oven_factory$new()

# Call the cook() method for 1 second
a_fancy_microwave$cook(1)









# ||| Over-overriding the cooking capabilities |||
# Explore other microwaves
microwave_oven_factory
fancy_microwave_oven_factory

# Define a high-end microwave oven class
high_end_microwave_oven_factory <- R6Class(
  "HighEndMicrowaveOven",
  inherit = fancy_microwave_oven_factory,
  public = list(
    cook = function(time_seconds){
      super$super_$cook(time_seconds)
      message(ascii_pizza_slice)
    }  
  )
)




# Instantiate a high-end microwave oven
a_high_end_microwave <- high_end_microwave_oven_factory$new()

# Use it to cook for one second
a_high_end_microwave$cook(1)







