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

(define-record-type c:card
    (c:%make-card comps)
    c:card?
  (comps c:%card-get-components c:%card-set-components!))

; for debugging
(define (c:print-card card)
  (write-line (list 'card: (c:%card-get-components card))))


#|  Components and fields will be represented with certain
list types; these are some syntactic functions.
|#

(define (c:component-field? object)
  (and (pair? object)
       (symbol? (car object))
       (= (length object) 2)))
(define c:component-field-name car)
(define c:component-field-value cadr)

(define (c:component? object)
  (and (pair? object)
       (symbol? (car object))
       (alist? (cadr object))
       (every c:component-field? (cadr object))))
(define c:component-name car)
(define c:component-fields cadr)

(define (c:component-list? object)
  (and (pair? object)
       (every c:component? object)))

;;;     Card Instantiator
#|  Creates a function that accepts a card collection and
adds a card with the given components to it, allowing 
multiple copies of a card to be created efficiently. 
Component-list should be a list of components, each in the
form returned by a function created using
component-instantiator.
|#

(define (c:card-instantiator component-list)
  (guarantee c:component-list? component-list)
  (define (new-card-to collection)
    (guarantee c:collection? collection)
    (let ((new-card (c:%make-card component-list)))
      (c:add-cards! collection (list new-card))))
  new-card-to)

;;;     Component Instantiator
#|  Creates a function that accepts an appropriate number of
field values and creates a properly formatted list for input
into card-instantiator. Component-name should be a symbol,
and field-list should be a list of unique symbols naming
each field. The resulting format is an alist:
(component-name ((field value) (field value) ...))
|#

(define (c:field-name-list? object)
  (and (list? object)
       (every symbol? object)))

(define (c:component-instantiator component-name field-name-list)
  (guarantee symbol? component-name)
  (guarantee c:field-name-list? field-name-list)
  (define (instantiate-component . values)
    (guarantee list? values)
    (if (= (length field-name-list) (length values))
	(let ((field-list (map
			   (lambda (name val) (list name val))
			   field-name-list values)))
	  (list component-name field-list))
	(error "incorrect number of field values" values)))
  instantiate-component)

;;;     Card Functionality
#|  Functions to allow for inspection and mutation
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


#|       Testing        |#
; dummy collection stuff
(define (c:collection? x) #t)
(define (c:add-cards! col cards)
  (for-each c:print-card cards))




