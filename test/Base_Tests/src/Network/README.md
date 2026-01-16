The Email_Spec tests today must be run manually using

runEngineDistribution --run /enso/test/Base_Tests/src/Network/Email_Spec.enso

Prioir to running you need to provide an configured provider to test_provider
and update plain_from_address = 'from@enso.org' from_address = ..Address
'from@enso.org' 'From User' to_address_base = 'to.address' to actual addresses
your provider can send from and you can receive emails to.

Then run the tests and check through the emails you receive match their subject
description.
