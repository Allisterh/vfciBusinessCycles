---
title: "BCA Replication: Classical VAR IRFs"
author: Matthew DeHaven
date: July 13, 2023
output: 
    github_document
---

# What is being replicated
The goal is to replicate Figure 20 of the Online Appendix, copied below.

<div class="figure">
<img src="/Users/matthewdehaven/Research/Projects/vfciBusinessCycles/./data-raw/figs-raw/bca-replication/figure20.png" alt="Figure 20: Impulse Response Functions to the MBC Shock: Bayesian vs Classical Inference" width="100%" />
<p class="caption">Figure 20: Impulse Response Functions to the MBC Shock: Bayesian vs Classical Inference</p>
</div>


In particular, the Orange line in the figure above shows the Classical VAR, the black line shows the Bayesian VAR.
Both are targetting the business cycle frequency domain for unemployment.
The orange band shows the 68% bootstrapped confidence interval.
The orange line *actually* shows the median of the boostrapped VAR IRFs, but I have pulled the non-bootstrapped IRF for comparison below. 
The median is very close (see below).

Yet to be implemented: bootstrapping the replicated VAR.

# Replication
