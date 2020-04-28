;;;;                Card Game Project - Card Representation

#|
Cards are essentially collections of various components, which
in turn are collections of fields that can be specified as
needed for a given type of card. Part of the power of components
is that by ensuring that the same "type" of component always
has the same field, assumptions can be made more effectively
about the types of things that can be done with that card.
|#

;;;     Card Representation
#|  The low-level representation for a card. Keeps track of
the associated components and their fields, maintaining the
invariant that every component identifier is unique.
|#

(define c:card
  'todo)


;;;     Card Instantiator
#|  Creates a function that accepts a card collection and
adds a card with the given components to it, allowing 
multiple copies of a card to be created efficiently. 
Component-list should be a list of components, each in the
form returned by a function created using
component-instantiator.
|#

(define (c:card-instantiator component-list)
  'todo)


;;;     Component Instantiator
#|  Creates a function that accepts an appropriate number of
field values and creates a properly formatted list for input
into card-instantiator. Component-name should be a symbol,
and field-list should be a list of unique symbols naming
each field. The resulting format is an alist:
(component-name ((field value) (field value) ...))
|#

(define (c:component-instantiator component-name field-list)
  'todo)


;;;     Card Functionality
#|  Functions to allow for mutation and inspection of card
objects.
|#

;; Checks if the card has a component with the given name.
(define (c:has-component? card component-name)
  'todo)

;; Adds the given component to the card if one with the same
;;   name is not already present. Returns false if there
;;   was already a component of the same name.
(define (c:add-component! card instantiated-component)
  'todo)

;; Removes a component with a given name from the card.
;;   Returns false if no such component was found.
(define (c:remove-component! card component-name)
  'todo)

;; Gets the value of a field within a given component.
(define (c:get-field-value card component-name field-name)
  'todo)

;; Changes the value of a field within a given component.
(define (c:set-field-value! card component-name field-name
			    new-value)
  'todo)
