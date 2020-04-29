;;;;                Card Game Project - Card Representation

#|
Cards are essentially collections of various components, which
in turn are collections of fields that can be specified as
needed for a given type of card. Part of the power of components
is that by ensuring that the same "type" of component always
has the same field, assumptions can be made more effectively
about the types of things that can be done with that card.
|#

#|
TODO: allow components to have type-checking predicates
for their fields?
|#

;;;     Card Representation
#|  The low-level representation for a card. Keeps track of
the associated components and their fields, maintaining the
invariant that every component identifier is unique. There
are records for components and fields to allow them to be
mutated without updating a card's entire component list.
|#


;; Cards
(define-record-type c:card
    (c:%make-card comps)
    c:card?
  (comps c:%card-components c:%card-set-components!))

;; Components
(define-record-type c:component
    (c:%make-component name fields)
    c:component?
  (name c:%component-name)
  (fields c:%component-fields))

;; Fields
(define-record-type c:component-field
    (c:%make-component-field name value)
    c:component-field?
  (name c:%component-field-name)
  (value c:%component-field-value c:%component-field-set-value!))



;; Printing functions for debugging
(define (c:print-card card)
  (write (list 'card:
	       (map c:%print-component
		    (c:%card-components card))))
  (newline))

(define (c:%print-component component)
  (list (c:%component-name component)
	(map c:%print-component-field
	     (c:%component-fields component))))

(define (c:%print-component-field field)
  (list (c:%component-field-name field)
	(c:%component-field-value field)))

;;;     Card Instantiator
#|  Creates a function that accepts a card collection and
adds a card with the given components to it, allowing
multiple copies of a card to be created efficiently.
Component-list should be a list of components.
|#

(define (c:component-list? object)
  (and (list? object)
       (every c:component? object)))

(define (c:card-instantiator component-list)
  (guarantee c:component-list? component-list)
  (define (new-card-to collection)
    (guarantee c:collection? collection)
    (let ((new-card (c:%make-card component-list)))
      (c:%add-cards! collection (list new-card))))
  new-card-to)

;;;     Component Instantiator
#|  Creates a function that accepts an appropriate number of
field values and creates a component with the provided fields.
Component-name should be a symbol, and field-list should be a
list of unique symbols naming each field.
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
			   (lambda (name val)
			     (c:%make-component-field name val))
			   field-name-list values)))
	  (c:%make-component component-name field-list))
	(error "Incorrect number of field values:" values)))
  instantiate-component)

;;;     Card Functionality
#|  Functions to allow for inspection and mutation
objects.
|#

;; Helper functions
(define (c:%find-component-by-name component-list component-name)
  (guarantee c:component-list? component-list)
  (guarantee symbol? component-name)
  (find (lambda (c)
	  (eq? (c:%component-name c) component-name))
	component-list))

(define (c:component-field-list? object)
  (and (list? object)
       (every c:component-field? object)))

(define (c:%find-field-by-name component-fields field-name)
  (guarantee c:component-field-list? component-fields)
  (guarantee symbol? field-name)
  (find (lambda (f)
	  (eq? (c:%component-field-name f) field-name))
	component-fields))

;; Checks if the card has a component with the given name.
(define (c:has-component? card component-name)
  (guarantee c:card? card)
  (guarantee symbol? component-name)
  (let ((component-list (c:%card-components card)))
    (c:%find-component-by-name component-list component-name)))

;; Adds the given component to the card if one with the same
;;   name is not already present. Returns false if there
;;   was already a component of the same name.
(define (c:add-component! card instantiated-component)
  (guarantee c:card? card)
  (guarantee c:component? instantiated-component)
  (if (c:has-component? card
			(c:%component-name instantiated-component))
      #f
      (let ((component-list (c:%card-components card)))
	(c:%card-set-components!
	 card
	 (cons instantiated-component component-list)))))

;; Removes a component with a given name from the card.
;;   Returns false if no such component was found.
(define (c:remove-component! card component-name)
  (guarantee c:card? card)
  (guarantee symbol? component-name)
  (if (c:has-component? card component-name)
      (let ((component-list (c:%card-components card)))
	(c:%card-set-components!
	 card
	 (remove (lambda (component)
		   (eq? (c:%component-name component)
			component-name))
		 component-list)))
      #f))

;; Gets the value of a field within a given component.
(define (c:get-field-value card component-name field-name)
  (guarantee c:card? card)
  (guarantee symbol? component-name)
  (guarantee symbol? field-name)
  (if (c:has-component? card component-name)
      (let* ((component-list (c:%card-components card))
	     (component (c:%find-component-by-name component-list
						   component-name))
	     (field (c:%find-field-by-name (c:%component-fields component)
					   field-name)))
	(if field
	    (c:%component-field-value field)
	    (error "Field not found in card component:"
		   card component-name field-name)))
      (error "Component not found in card:" card component-name)))

;; Changes the value of a field within a given component.
(define (c:set-field-value! card component-name field-name
			    new-value)
  (guarantee c:card? card)
  (guarantee symbol? component-name)
  (guarantee symbol? field-name)
  (if (c:has-component? card component-name)
      (let* ((component-list (c:%card-components card))
	     (component (c:%find-component-by-name component-list
						   component-name))
	     (field (c:%find-field-by-name (c:%component-fields component)
					   field-name)))
	(if field
	    (c:%component-field-set-value! field new-value)
	    (error "Field not found in card component:"
		   card component-name field-name)))
      (error "Component not found in card:" card component-name)))


#|       Testing
; dummy collection stuff
(define (c:collection? x) #t)
(define test-coll '())
(define (c:%add-cards! coll cards)
  (set! test-coll (append test-coll cards)))

;; Card/component instantiators

(define basic-comp
  (c:component-instantiator 'basic
			    '(rank suit)))
(define flip-comp
  (c:component-instantiator 'flip
			    '(faceup)))

((c:card-instantiator (list (basic-comp 5 'H)))
 test-coll)
((c:card-instantiator (list (basic-comp 2 'S)))
 test-coll)
(for-each c:print-card test-coll)
        ; -> (card: ((basic ((rank 5) (suit h)))))
        ; -> (card: ((basic ((rank 2) (suit s)))))


;; Inspectors/mutators

(define test-card (car test-coll))
(c:has-component? test-card 'basic)
        ; -> #t [or the component object]
(c:has-component? test-card 'flip)
        ; -> #f

(c:add-component! test-card (flip-comp #t))
(c:has-component? test-card 'basic)
        ; -> #t [or the component object]
(c:has-component? test-card 'flip)
        ; -> #t [or the component object]

(c:remove-component! test-card 'basic)
(c:has-component? test-card 'basic)
        ; -> #f
(c:has-component? test-card 'flip)
        ; -> #t [or the component object]

(c:add-component! test-card (basic-comp 5 'H))
(c:get-field-value test-card 'basic 'rank)
        ; -> 5
(c:get-field-value test-card 'basic 'suit)
        ; -> 'H
(c:get-field-value test-card 'flip 'faceup)
        ; -> #t

(c:set-field-value! test-card 'flip 'faceup #f)
(c:get-field-value test-card 'flip 'faceup)
        ; -> #f

(c:print-card test-card)
        ; -> (card: ((basic ((rank 5) (suit h))) (flip ((faceup #f)))))
|#
