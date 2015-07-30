### A different take on the object model

## Why can't objects just talk to each other?

I was making a game and I wanted a monster to track the player through a
dungeon room. Why couldn't the monster just "hear" the player's movements? 

If I'm in the room with someone, I can hear and see them moving. That person is
communicating their movements to me without ever having to explicity say
anything to me personally. If they start to move too closely I can get out of
the way.

Why can't the monster and player do the same? This project is my solution to
that question.

## How do they talk?

Before I explain how they talk, I want to explain the structure of the project.
Right now, the core data structure is a tree. Each node on this tree represents
an object. I call these nodes branches.

Each branch has any number of components (like an entity-component system) that
define that branch. I call those components leaves.

Each branch has any number of leaves which define it. And each branch also has
any number of children.

That means a tree is simply a collection of branches.

## That's cool, but how do they talk?

Since each branch is defined by its leaves (rather, each object is defined by 
its components), the leaves do all of the work.

At its very core, the leaf is an object which has methods. The methods it has
are the messages it receives.

For example, the player mashes the up-arrow on their keyboard. It produces a
message that looks something like this:

    (message 'input 'up)

As an example, let's say we have a leaf that looks like this:

    (handle-message input (direction)
        "Translate the player's input to our player object."
        (think 'move-towards (input->coordinates direction))

The "think" procedure tells Tree-Talk that we want that message to be internal.
In other words, "think" tells the leaf to message the other leaves of its 
branch.

Let's take this example further and look at another leaf:

    (handle-message move-towards (coordinates)
        "Move towards destination every update."
        (setf (property 'destination) coordinates))

    (handle-message update (dt)
        "Move the object and broadcast our location."
        (move-delta dt)
        (broadcast 'movement (property 'location)))

This leaf accepted the 'move-towards message and set its destination. Then at
every 'update it moves and then broadcasts its current location.

## What are the ways you can send messages?

Right now there are just four ways:

* Broadcast - Send a message to the entire tree.
* Think - Send a message internally to the other leaves on its branch.
* Reply - Send a message directly to the author of the message it received.
* Command - Send a message to its branch's children.

## How do I use this?

Just clone the repo and pop "treepost.lisp" into your interpreter. If you want
to play around `treepost` is the procedure you're looking for. `(treepost 
'update)` will spit out some information.

`(treepost 'start)` will return a message. The entire point of the tree is that
you send messages in and receive responses, which are just messages you can put
right back into the tree.

## The future

I have plans to make this a library or standalone program extensible by Common
Lisp. Probably both.

I am using Embeddable Common Lisp as my interpreter as I hope to embed this in
a C program to use as my object model for some games. I think this idea can be
really useful outside of games, though.
