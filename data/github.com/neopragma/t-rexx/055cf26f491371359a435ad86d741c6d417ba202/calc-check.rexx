/* test script to demonstrate the rexx unit test framework */

context('Checking the calc function')
check( 'Adding 3 and 4',        expect( calc( 3,  '+', 4 ),  'to be',      6  ))
check( 'Adding 5 and 2',        expect( calc( 5,  '+', 2 ),  'to be',      7  ))
check( 'Subtracting 3 from 10', expect( calc( 10, '-', 3 ),  'to be',      7  ))
check( 'Multiplying 15 and 2',  expect( calc( 15, '*', 2 ),  'to be',     31  ))
check( 'Dividing 3 into 15',    expect( calc( 15, '/', 3 ),  'not to be', 13  ))
