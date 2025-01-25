# Rendering Graphs

**Abstract.** This guide covers rendering directed acyclic graphs as trees, and cyclic graphs as webs. We provide a description, a reference implementation, and visual examples, for - common algorithms.

**Keywords:** Graphs, Algorithms, SGD, Stress Majorization.

_August 12, 2024 — August 13, 2024_

# Background

Visual design is important. To illustrate, consider the two images shown below. Which one(s) have a path from $A$ to $E$?

![] ![]
<Graph>
    node A []
    node B [right of A]
    node C [right of B]
    node D [right of C]

    node E [below of A]
    node F [right of E]
    node G [right of F]
    node H [right of G]

    A --> B
    A --> C
</Graph>
<Graph>
</Graph>

These two images actually depict the same graph. Despite this, one highlights useful information, and the other obscures useful information.

So, we will need to consider visual design in our algorithms. 

This is actually why rendering graphs is so hard. It is often unclear how to mathematically predict the clarity of an image. Nonetheless, we will try.

To start, let's break up our problem space.

A graph is **directed** when the edges between nodes only go in one direction. For clarity, I will render directed graphs with little arrows.

![] ![]

A graph is **acyclic** if there are no nodes that have a path to themselves.

![] ![]

So, if a graph is both directed and acyclic, we call it a _directed acyclic graph_, or a **DAG**.

DAGs are typically rendered as trees, like the one shown below.

![]

On the other hand, we often render cyclic graphs as webs.

![] ![]

There are other ways to render cyclic graphs, but this guide will focus on webs.

<!-- ![] ![] ![] -->

So, continuing, DAGs are typically rendered using a simple branching algorithm.

On the other hand, cyclic graphs are often rendered using **Stress Majorization** with \_\_\_\_\_. However, **Stochastic Gradient Descent** has been shown to provide similar quality while being slightly simpler.

In the next two sections, I will describe how these algorithms work in detail, provide reference implementations, and present example results.

# Rendering DAGs 

# Rendering Cyclic Graphs

## Stress Majorization 

## Priming

## Stochastic Gradient Descent

# Conclusions


# References
