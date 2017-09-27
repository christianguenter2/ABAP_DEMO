REPORT z_test_mesh.

TYPES: BEGIN OF ty_order,
         order_number TYPE char10,
         customer     TYPE char10,
       END OF ty_order,
       tty_order TYPE HASHED TABLE OF ty_order WITH UNIQUE KEY order_number.

TYPES:   BEGIN OF ty_customer,
           customer TYPE char10,
           name     TYPE char30,
           city     TYPE char30,
           route    TYPE char10,
         END OF ty_customer,
         tty_customers TYPE STANDARD TABLE OF ty_customer WITH NON-UNIQUE KEY customer.


TYPES: BEGIN OF ty_route,
         route      TYPE char10,
         route_name TYPE char20,
       END OF ty_route,
       tty_route TYPE HASHED TABLE OF ty_route WITH UNIQUE KEY route.


TYPES: BEGIN OF MESH ty_rep_mesh,
         order_info    TYPE tty_order
                       ASSOCIATION order_to_customer TO customer_info
                       ON customer = customer,
         customer_info TYPE tty_customers
                       ASSOCIATION customer_to_route TO route_info
                       ON route = route,
         route_info    TYPE tty_route,
       END OF MESH ty_rep_mesh.

DATA(customers) = VALUE tty_customers(
                    ( customer = 'c0001' name = 'Test Customer 1' city = 'NY'  route = 'R0001' )
                    ( customer = 'c0002' name = 'Customer 2'      city = 'LA'  route = 'R0003' )
                    ( customer = 'c0003' name = 'Good Customer 3' city = 'DFW' route = 'R0001' )
                    ( customer = 'c0004' name = 'Best customer 4' city = 'CH'  route = 'R0003' ) ).

DATA(orders) = VALUE tty_order(
                    ( order_number = '1' customer = 'c0001' )
                    ( order_number = '2' customer = 'c0002' )
                    ( order_number = '3' customer = 'c0001' )
                    ( order_number = '4' customer = 'c0003' )
                    ( order_number = '5' customer = 'c0004' )
                    ( order_number = '6' customer = 'c0002' ) ).

DATA(routes) = VALUE tty_route(
                    ( route = 'R0001' route_name = 'Route 1' )
                    ( route = 'R0002' route_name = 'Route 2' )
                    ( route = 'R0003' route_name = 'Route 3' ) ).

DATA mesh TYPE ty_rep_mesh.

mesh-order_info    = orders.
mesh-customer_info = customers.
mesh-route_info    = routes.

LOOP AT mesh-order_info ASSIGNING FIELD-SYMBOL(<order>).
  WRITE: / <order>-order_number.

  DATA(customer) = mesh-order_info\order_to_customer[ mesh-order_info[ order_number = <order>-order_number ] ].
  DATA(route)    = mesh-order_info\order_to_customer[ mesh-order_info[ order_number = <order>-order_number ] ]\customer_to_route[ ].

  WRITE: | { customer-customer } { customer-name } { route-route } { route-route_name }|.
ENDLOOP.
