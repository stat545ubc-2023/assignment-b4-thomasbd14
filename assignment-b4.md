STAT 545 Assignment B4
================
Thomas Deckers
13/11/2023

    ## Warning: package 'janeaustenr' was built under R version 4.3.2

    ## Warning: package 'stopwords' was built under R version 4.3.2

# STAT 545 Assignment B4 option A: Strings and Functional Programming

## Exercise 1: Word counts in Jane Austen

### Intro

For this exercise, we’d like to make a plot of the most common words
appearing in Jane Austen’s *Emma*, exluding stopwords such as “the”,
“a”, and “and”.

### Data

Let’s inspect the data, provided by the `janeaustenr` package:

``` r
typeof(emma)
```

    ## [1] "character"

``` r
head(emma, n = 16)
```

    ##  [1] "EMMA"                                                                
    ##  [2] ""                                                                    
    ##  [3] "By Jane Austen"                                                      
    ##  [4] ""                                                                    
    ##  [5] ""                                                                    
    ##  [6] ""                                                                    
    ##  [7] ""                                                                    
    ##  [8] "VOLUME I"                                                            
    ##  [9] ""                                                                    
    ## [10] ""                                                                    
    ## [11] ""                                                                    
    ## [12] "CHAPTER I"                                                           
    ## [13] ""                                                                    
    ## [14] ""                                                                    
    ## [15] "Emma Woodhouse, handsome, clever, and rich, with a comfortable home" 
    ## [16] "and happy disposition, seemed to unite some of the best blessings of"

As we can see, we have a character array, where each entry is a line of
the text.

Our other input is the list of stopwords from the `stopwords` package.

``` r
stopwds <- stopwords("en")

typeof(stopwds)
```

    ## [1] "character"

``` r
head(stopwds)
```

    ## [1] "i"      "me"     "my"     "myself" "we"     "our"

This is stored in a very similar format.

### Proprocessing

First thing’s first, we want to transform the format of Emma into an
array of individual words. This will require first removing the empty
elements, and all the punctuation.

``` r
transformed_emma <- emma %>%
  str_replace_all("[:punct:]|[:symbol:]|[:digit:]", " ") %>% # Remove punctuation, numbers, and symbols
  tolower() %>% # Remove all capitalization
  str_split(" ") %>% # Split on the words
  flatten() %>% # Remove the lines as they previously were
  unlist() # make it a character array

#lastly, remove the empty entries
transformed_emma <- transformed_emma[transformed_emma != ""] # Remove the blank lines

head(transformed_emma, n = 30)
```

    ##  [1] "emma"        "by"          "jane"        "austen"      "volume"     
    ##  [6] "i"           "chapter"     "i"           "emma"        "woodhouse"  
    ## [11] "handsome"    "clever"      "and"         "rich"        "with"       
    ## [16] "a"           "comfortable" "home"        "and"         "happy"      
    ## [21] "disposition" "seemed"      "to"          "unite"       "some"       
    ## [26] "of"          "the"         "best"        "blessings"   "of"

Here, I replaced punctuation with spaces. This occasionally divides
words which are arguably a single word, such as “twenty-one”, and
“long-standing”. The easy alternative is to remove punctuation outright.
However, this creates the issue that words that are only separated by
punctuation marks—most commonly an emdash in Emma—end up being treated
as one word, e.g. marksmost and Emmaend in this sentence. Neither of
these solutions is ideal, but I prefer dividing words rather than adding
completely non-sensical words to the count.

### Counting and filtering

Now we need to generate a count. Luckily, dplyr provides a convenient
function for this if we transform our data into a tibble. Dplyr also
provides a convenient way to filter out the stopwords.

``` r
emma_counts <- tibble("word" = transformed_emma) %>%
  count(word)

emma_counts <- emma_counts %>%
  filter(!word %in% stopwords("en")) %>%
  arrange(desc(n))

head(emma_counts, n = 10)
```

    ## # A tibble: 10 × 2
    ##    word        n
    ##    <chr>   <int>
    ##  1 mr       1154
    ##  2 s         933
    ##  3 emma      865
    ##  4 mrs       701
    ##  5 miss      602
    ##  6 must      571
    ##  7 harriet   506
    ##  8 much      486
    ##  9 said      484
    ## 10 one       458

We can see a few things that are not great here. “Mr,” “Ms”, “Mrs”,
“Miss”, “Harriet”, and “Emma” all come from addressing characters. This
doesn’t really tell us much about Austen’s word choice. Then of course,
“s” shows up from contractions where the apostrophe was replaced with a
space, as discussed above. Let’s remove all of these words as well,
along with some more proper nouns that turn up near the top.

``` r
manual_word_blacklist <- c("mr", "s", "mrs", "miss", "harriet", "emma",
                           "weston", "knightley", "elton", "woodhouse", "jane",
                           "fairfax", "churchill", "hartfield", "frank")

emma_counts <- emma_counts %>%
  filter(!word %in% manual_word_blacklist)

head(emma_counts, n = 20)
```

    ## # A tibble: 20 × 2
    ##    word        n
    ##    <chr>   <int>
    ##  1 must      571
    ##  2 much      486
    ##  3 said      484
    ##  4 one       458
    ##  5 every     435
    ##  6 well      403
    ##  7 thing     399
    ##  8 think     384
    ##  9 little    361
    ## 10 good      359
    ## 11 never     358
    ## 12 know      337
    ## 13 might     326
    ## 14 now       313
    ## 15 say       312
    ## 16 can       284
    ## 17 quite     282
    ## 18 time      280
    ## 19 great     265
    ## 20 nothing   256

There’s another approximation that has to be made here. Emma contains a
character named “Frank”. Presumably, a large share of the occurrences of
the word “frank” refers to this character as opposed to the English
adjective, so I am removing it from the list. However, we do also lose
any usage of the adjective (or noun, if Emma ever enjoys a german-style
sausage!). A more sophisticated approach might have used regex to count
lower case occurrences of the word “frank”, although this would have
missed instances that occur at the start of the sentence.

### Plotting

The simplest way to view this is as a column plot of the top few words.

``` r
emma_counts %>% head(n=20) %>%
  mutate(word = factor(word, levels = word)) %>%
  ggplot(aes(x=word, y=n)) +
  geom_col() +
  labs(x = "Word", y = "Number of Occurences")
```

![](assignment-b4_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

## Exercise 2: Modified Pig Latin

Here, I will create a function that takes in a sentence, and turns it
into a modified version of Pig Latin. My version of piglatin will have
the following rules

1.  Leave words with two letters or less unmodified
2.  Move the last consonant and any trailing vowels to the start of the
    word
3.  If a word starts and ends with the same consonant, add an o between
    the part now moved to the front and the original start of the word
4.  Add an “s” to the start of the word, if we haven’t already moved an
    s to the start

I’ll first define a helper function that takes only a single word of
length greater than 2, to do the heavy lifting.

``` r
#' Piglatin word
#' 
#' @description This is a helper function for `my_piglatin`. It is not intended
#' to be called independently. It takes in a word and returns the my-piglatin-ized
#' version of the word. See `my_piglatin` for details.
#' 
#' @param word a string not containing any whitespace (spaces, tabs, newlines)
#' 
#' @return a string of the provided `word` translated into my piglatin
#' 
#' @importFrom stringr str_detect str_locate str_sub str_c
#' 
#' @examples .piglatin_word("test") # returns stotes 
.piglatin_word <- function(word){
  
  # select the case of the s to add, based on the first letter of the word
  s_case <- ifelse(str_detect(word, "^[:upper:]"), "S","s")
  
  word <- tolower(word)
  
  suffix_loc <- str_locate(word, "[^a,e,i,o,u,y][a,e,i,o,u,y]*$") # Find the last consonant and trailing vowels
  suffix = str_sub(word,suffix_loc[1],suffix_loc[2]) # save the string of the suffix itself
  
  str_c(s_case,  # Add an s to the start of the word
        ifelse(suffix != "s", suffix, ""), # Add ending of the word, unless it's just s
        ifelse(str_detect(word, "^([^a,e,i,o,u,y]).*\\1$"),"o", ""), # If the word starts and ends with the same letter, add an o in between
        str_sub(word,0,suffix_loc[1]-1)) # And finally add the rest of the word
}
```

Now we can document and write a function that applies this to full
sentences, and handles all the error checking.

``` r
#' My Piglatin
#' 
#' @description Takes in a word or sentence and converts it into my fun version
#' of piglatin. The transformation is summarized by these 4 rules:
#'   1. Leave words with two letters or less unmodified
#'   2. Move the last consonant and any trailing vowels to the start of the word
#'   3. If a word starts and ends with the same consonant, add an o between the 
#'     part now moved the front and the original start of the word
#'   4. Add an "s" to the start of the word, if we haven't already moved an s to 
#'     the start
#' This particular implementation will capitalize the leading S if an input word
#' had a capital letter at the start, but will otherwise make all letters
#' lowercase. Non-alphabetical characters are unmodified.
#' 
#' @param text A string containing the text to be transformed. The string may
#' consist of multiple words, but must not contain any newlines
#' 
#' @return the my-piglatin-ized version of the provided `text`
#' 
#' @export 
#' 
#' @importFrom stringr str_replace_all str_detect
#' 
#' @examples my_piglatin("Hello, world!") # returns "Slohel, sdworl!"
#' @examples my_piglatin("Antidisestablismentarianism") #returns Smantidisestablismentarianis
#' 
my_piglatin <- function(text) {
  ## Input tests ##
  
  # Check for nulls
  if(is.null(text)){
    stop("Error: check input text, cannot operate on null")
  }
  # Ensure input is a string
  if(!(is.character(text) & length(text) == 1)){
    stop("Error: incorrect input type received, please provide a single string")
  }
  # Check for newlines
  if(str_detect(text, "\n")){
    stop("Error: Text may not contain newlines")
  }
  
  
  ## Function execution ##
  str_replace_all(text,"([:alpha:]{3,})", .piglatin_word)
}
```

Now, let’s write a few basic tests

``` r
# Build up the regular usage cases
test_that("Regular use",
          {
            expect_identical(my_piglatin("piglatin"), "snpiglati") # simple case
            expect_identical(my_piglatin("Capital"), "Slcapita") # upper case at start of word
            expect_identical(my_piglatin("mate"), "stema") # trailing vowels
            expect_identical(my_piglatin("test"), "stotes") # Same consonant at beginning and end of word
            expect_identical(my_piglatin("bass"), "sbas") # 's' at end of word
            expect_identical(my_piglatin("hi"), "hi") # Ignore two letter words
            expect_identical(my_piglatin("1234567890!@#$%^&*()-={}:|<>?"), "1234567890!@#$%^&*()-={}:|<>?") # Ignore non-alphabetical
            expect_identical(my_piglatin("'ab'"), "'ab'") # Special check for single quotes
            expect_identical(my_piglatin('"ab"'), '"ab"') # Special check for double quotes
            expect_identical(my_piglatin("This line is to test 'my_piglatin'!"), "Sthi sneli is to stotes 'my_snpiglati'!") # Put it all together in a sentence
          })
```

    ## Test passed 😀

``` r
# Test for error conditions

test_that("Error cases",
          {
            expect_error(my_piglatin(NULL), "Error: check input text, cannot operate on null")
            expect_error(my_piglatin(0), "Error: incorrect input type received, please provide a single string")
            expect_error(my_piglatin(c("array", "of", "words")),"Error: incorrect input type received, please provide a single string" )
            expect_error(my_piglatin(list("list", "of", "words")),"Error: incorrect input type received, please provide a single string" )
            expect_error(my_piglatin("text
                                     with nexline"), "Error: Text may not contain newlines")
          })
```

    ## Test passed 🌈
