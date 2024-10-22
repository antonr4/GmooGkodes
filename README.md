<!-- README.md is generated from README.Rmd. Please edit that file -->

# GmooGcode

This repository contains the R code used to produce the graphics in the
book “Getting (more out of) Graphics”.

## Datasets

The four dataset packages accompanying the book (GmooG, ComradesM,
FilmsGmooG, ChessGmooG) are on CRAN.

## Coding language

The graphics have almost all been produced using R and that part of R
known as the tidyverse. Many of the graphics could be drawn just as well
with other software, perhaps better.

There is a file for each chapter with graphics. The code for Chapters 2
to 26 (in Part I of the book) is self-contained, all the code needed is
in the same chapter, although individual code chunks often depend on
earlier code in a chapter. Most of the code in Chapters 27 to 33 (in
Part II of the book) depends on code in chapters in Part I. Backward
references are provided in the chunk headers.

## Named objects

R objects that are used later on in the book, particularly graphics and
subsets of data, have been given names where they originate to avoid
repeating code.

## Sizes of graphics

The graphics drawn in the book have sometimes been specially sized to
fit with the layout and text. So graphics drawn with the code in this
repository may have to be resized to match what you see in the book.

## Warnings and messages

Some warning messages are produced by R if cases are missing. This is
usually not important, but it can be helpful to be reminded that not all
cases are displayed in a plot.

The output of R packages may include messages making explicit features
that have been used implicitly. Examples include listing which variables
have been used to merge files and what smoothing function has been
applied.

## Qualification

The code is provided to enable others to reproduce the graphics or, even
better, to help them to produce their own versions. Users should note
what it says in Chapter 1: “There is no claim that the R code
accompanying the book is to be recommended, but it may be helpful.”
