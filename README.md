# rationale

from this thread on racket mailing list

https://groups.google.com/forum/?hl=en-GB#!topic/racket-users/nj_PZX5zyeI

this is the exercise of lab 7, how to desgin program

http://www.ccs.neu.edu/course/cs2500f16/lab7.html

# log

markov memory 1: sentence. but little meaning

markov memory 2: better

markov memory 3: replicate sentence from the text. maybe because the text sample is small. so the possibility runs out very fast after a few initial branches

memory 2 is in between: fragments of the text, but mixed up

generate sentence backward

# functionality
(say-something)
(say-something-about "flower")


(ask-something)
> what ... ?

- i answer ...

- with my answer, scan for keywords, to respond with the next sentence

- when i ask, use match (the bot project) to scan for keywords (list-no-order), and respond with default answer. or generate a new one.

# to-do

have separate data of "questions", to create command:
(ask-me-something)

