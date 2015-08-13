### A different take on the object model

## What's the down and dirty?

I'm not a biologist, but I don't think cells operate by just reaching into
another cell with 'getters' and 'setters' and messing with information. They
communicate. They talk to each other. 

And objects just don't talk to each other in programming.

This project is my vision of what I understood 'object-oriented' and 'message
passing' to be before I learned object-oriented programming:

* An object acts alone and in an entirely insulated environment. 
* An object only acts by the messages it receives.
* An object may act on a message by sending a response (which is yet another message) or it may do 
everything internally without response.

I think using Lisp allowed me to get right to the fundamentals.

I tried doing [virtual methods and templates in C++](https://github.com/rlt3/Messaging/blob/master/src/messageable.hpp "c++ mess"). 
Before that (and before I realized I was ever trying to solve this problem) [I was
using PHP in a similar way.](https://github.com/rlt3/Stream "sphagetti php")

## Why can't objects just talk to each other?

I was making a game and I wanted a monster to track the player through a
dungeon room. I kept having a bunch of circular dependency issues with the
player being an entity and the monster being and entity and blah blah blah. 

I knew the solution -- just use yet another class to interface between the two.
But why couldn't the monster just "hear" the player's movements? 

If I'm in the room with someone, I can hear and see them moving. That person is
communicating their movements to me without ever having to explicity say
anything to me personally. If they start to move too closely I can get out of
the way.

With Tree-Talk, the player and monster can just talk to each other and figure
all this out amongst themselves.

## How do they talk?

Before I explain how they talk, I want to explain the structure of the project.
Right now, the core data structure is a tree. I am using a different definition
of leaves and nodes.

Each node on this tree represents an object. I call these nodes branches.

Each branch has any number of properties (like an entity-component system) that
define that branch. I call those properties leaves.

Each branch has any number of leaves which define it. And each branch also has
any number of children.

That means a tree is simply a collection of branches.

## That's cool, but how do they talk?

Since this is an object system, a leaf is an object which has methods. The
methods it has are the messages it receives.

For example, the player mashes the up-arrow on their keyboard. It produces a
message that looks something like this:

    (message 'input 'up)

We might have a leaf that looks like this:

    (handle-message input (direction) input-leaf
        "Translate the player's input to our player object."
        (think 'move-towards (input->coordinates direction))

The "think" procedure tells Tree-Talk that we want that message to be internal
and messages the other leaves on that branch. In other words, it is talking to
itself.

Let's take this example further and look at another leaf:

    (handle-message move-towards (coordinates) movement-leaf
        "Set the destination."
        (property-set! 'destination coordinates))

    (handle-message update (dt) movement-leaf
        "Move with delta time and let everyone know our location every update."
        (move-delta dt)
        (say 'movement (property 'location)))

This leaf accepted the 'move-towards message and set its destination. Then at
every 'update it moves and then tells its current location to the other
branches. That's how I might handle collision.

## What are the ways you can send messages?

Right now there are just five ways:

* Say - Send a message to a branch's siblings.
* Reply - Send a message directly to the author of the message.
* Think - Send a message internally to the other leaves on a branch.
* Command - Send a message to a branch's children.
* Broadcast - Send a message to the entire tree.

## How do I mess around with this?

I have a general structure and some scripts already defined in "stage.lisp".
You can get started right away by loading that into your interpreter and 
calling `(tree-message *tree* 'update)`.

`(tree-message *tree* 'start)` will return a message. The entire point of the
tree is that you send messages in and receive responses, which are just
messages you can put right back into the tree.

## The future

Though I am using this for games, I think the general idea is bigger than
games and the reason why I want to share it with everyone.

I am attempting to make a [C Library which can interface with objects
written in many different languages](https://github.com/rlt3/tree-talk-c "tree-talk-c").
