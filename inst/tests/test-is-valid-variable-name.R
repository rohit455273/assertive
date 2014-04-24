                                        
test.is_valid_variable_name.x.returns_true <- function()
{
  checkTrue(is_valid_variable_name("x"))
}
                                         
test.is_valid_variable_name.dot.returns_true <- function()
{
  checkTrue(is_valid_variable_name("."))
}
                                           
test.is_valid_variable_name.2_dots.returns_true <- function()
{
  checkTrue(is_valid_variable_name(".."))
}
                                             
test.is_valid_variable_name.3_dots.returns_allow_reserved <- function()
{
  checkTrue(is_valid_variable_name("..."))             
  checkTrue(!is_valid_variable_name("...", allow_reserved = FALSE))
}
                       
test.is_valid_variable_name.4_dots.returns_true <- function()
{
  checkTrue(is_valid_variable_name("...."))
}
                       
test.is_valid_variable_name.5_dots.returns_true <- function()
{
  checkTrue(is_valid_variable_name("....."))
}

test.is_valid_variable_name.dash.returns_false <- function()
{
  checkTrue(!is_valid_variable_name("_"))
}

test.is_valid_variable_name.1_x.returns_false <- function()
{
  checkTrue(!is_valid_variable_name("1x"))
}
                                   
test.is_valid_variable_name.dot_dash.returns_true <- function()
{
  checkTrue(is_valid_variable_name("._"))
}
                                   
test.is_valid_variable_name.dot_1.returns_false <- function()
{
  checkTrue(!is_valid_variable_name(".1"))
}
                    
test.is_valid_variable_name.dot_x.returns_true <- function()
{
  checkTrue(is_valid_variable_name(".x"))
}
                    
test.is_valid_variable_name.dot_dash_1.returns_true <- function()
{
  checkTrue(is_valid_variable_name("._1"))
}
                    
test.is_valid_variable_name.dot_dot_1.returns_allow_reserved <- function()
{
  checkTrue(is_valid_variable_name("..1")) 
  checkTrue(!is_valid_variable_name("..1", allow_reserved = FALSE))
}
                                          
test.is_valid_variable_name.dot_dot_2.returns_allow_reserved <- function()
{
  checkTrue(is_valid_variable_name("..2")) 
  checkTrue(!is_valid_variable_name("..2", allow_reserved = FALSE))
}
                    
test.is_valid_variable_name.dot_dot_dot_1.returns_true <- function()
{
  checkTrue(is_valid_variable_name("...1"))
}
                    
test.is_valid_variable_name.dot_dot_dot_dot_1.returns_true <- function()
{
  checkTrue(is_valid_variable_name("....1"))
}
                    
test.is_valid_variable_name.dot_dot_x.returns_true <- function()
{
  checkTrue(is_valid_variable_name("..x"))
}
                  
test.is_valid_variable_name.long_name.returns_false <- function()
{
  vn <- paste(rep.int("a", 10001L), collapse = "")
  checkTrue(!is_valid_variable_name(vn))
}      
                 
test.is_valid_variable_name.same_names.returns_allow_duplicates <- function()
{
  vn <- rep.int("foo", 2)
  checkTrue(all(is_valid_variable_name(vn)))
  checkTrue(!all(is_valid_variable_name(vn, allow_duplicates = FALSE)))
}
