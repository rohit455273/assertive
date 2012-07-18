test.is_cas_number.a_character_vector.returns_true_when_string_contains_a_cas_number <- function()
{
  x <- c(
    water = "7732-18-5", 
    d_glucose = "50-99-7",
    l_glucose = "921-60-8",
    no_hyphens = "7732185", 
    two_check_digits = "7732-18-55",
    bad_check_digit = "7732-18-4"
  )
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_cas_number(x)
  )
} 


test.is_credit_card_number.valid_card_numbers.returns_true_for_all <- function()
{
  x <- c(
    #visa
    "4111 1111 1111 1111",
    "4012888888881881",
    #mastercard
    "5555 5555 5555 4444",
    "5105 1051 0510 5100",
    #amex
    "3782 822463 10005",
    "3714 496353 98431",
    "3787 344936 71000", 
    #diners
    "3056 930902 5904",
    "3852 000002 3237",
    #discover
    "6011 1111 1111 1117",
    "6011 0009 9013 9424",
    #jcb
    "3530 1113 3330 0000",
    "3566 0020 2036 0505"
  )  
  expected <- rep.int(TRUE, length(x))
  names(expected) <- x
  checkEquals(
    expected,
    is_credit_card_number(x)
  )
}

test.is_credit_card_number.invalid_card_numbers.returns_false_for_all <- function()
{
  x <- c(
    #visa
    "4111 1111 1111 11111",  #too many digits
    "4012888888881882",      #bad check digit
    #mastercard
    "5655 5555 5555 4443",   #starts 56
    "51051 051 0510 5100",   #bad spacing
    #amex
    "3782 822463 1005"       #not enough digits
  )  
  expected <- rep.int(FALSE, length(x))
  names(expected) <- x
  checkEquals(
    expected,
    is_credit_card_number(x)
  )
}


test.is_date_string.a_character_vector.returns_true_when_string_contains_a_date <- function()
{
  x <- c("1999-12-31 23:59:59", "1979-08-01 01:00:00", "31 Dec 1999 11:59:59PM", "not a date", "NA")
  expected <- c(TRUE, TRUE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_date_string(x)
  )
} 


test.is_email_address.a_character_vector_simple_match.returns_true_when_string_contains_an_email_address <- function()
{
  x <- c("foo@bar.com", "foo@@bar.com", "@bar.com", "foo@bar", "foo@bar.comma", "foo!@bar.com", "NA")
  expected <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_email_address(x)
  )
} 

test.is_email_address.a_character_vector_rfc2822_match.returns_true_when_string_contains_an_email_address <- function()
{
  x <- c("foo@bar.com", "foo@@bar.com", "@bar.com", "foo@bar", "foo@bar.comma", "foo!@bar.com", "NA")
  expected <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_email_address(x, method = "rfc2822")
  )
} 


test.is_hex_colour.a_character_vector.returns_true_when_string_contains_a_hex_colour <- function()
{
  x <- c(
    "#0123456", "#789abc", "#defDEF", #ok
    "012345",                         #no hash
    "g12345",                         #bad letter
    "#01 23 45",                      #contains spaces
  )
  expected <- rep(c(TRUE, FALSE), each = 3)
  names(expected) <- x
  checkEquals(
    expected,
    is_hex_colour(x)
  )
}


test.is_ip_address.a_character_vector.returns_true_when_string_contains_an_ip_address <- function()
{
  x <- c(   
    localhost     = "localhost", 
    valid_address = "255.0.255.0", 
    out_of_range  = "1.2.3.256",
    five_blocks   = "1.2.3.4.5",
    non_numeric   = "1.2.3.Z",
    missing_block = "1.2.3.NA"
  )
  expected <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_ip_address(x)
  )
} 


test.is_isbn_code.a_character_vector_type_10.returns_true_when_string_contains_an_isbn10_code <- function()
{
  x <- c(
    hyphens             = "0-387-98503-4",
    spaces              = "0 387 98503 4",
    just_numbers        = "0387985034",
    too_long            = "00-387-98503-4",
    too_short           = "0-387-9850-4",
    non_numeric         = "Z-387-98503-4",
    invalid_check_digit = "0-387-98503-5"
  )
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_isbn_code(x, type = "10")
  )
} 

test.is_isbn_code.a_character_vector_type_13.returns_true_when_string_contains_an_isbn13_code <- function()
{
  x <- c(
    hyphens             = "978-0-387-98503-9",
    spaces              = "978 0 387 98503 9",
    just_numbers        = "9780387985039",
    too_long            = "9978-0-387-98503-9",
    too_short           = "978-0-387-9850-9",
    non_numeric         = "Z78-0-387-9850-9",
    invalid_check_digit = "978-0-387-98503-8"
  )
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_isbn_code(x, type = "13")
  )
} 


test.is_missing_or_empty_character.a_character_vector.returns_true_when_string_is_missing_or_empty <- function()
{
  x <- c(
    missing      = NA_character_,
    empty        = "",
    non_empty    = "a",
    space        = " ",
    not_missing1 = "NA",
    not_missing2 = "<NA>"
  )
  expected <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_missing_or_empty_character(x)
  )
  checkEquals(
    !expected,
    is_not_missing_nor_empty_character(x)
  )
} 


test.is_numeric_string.a_character_vector.returns_true_when_string_contains_a_number <- function()
{
  x <- c("1", "-2.3e4", "Inf", "one", "NA")
  expected <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
  names(expected) <- x
  checkEquals(
    expected,
    is_numeric_string(x)
  )
} 


test.is_uk_car_licence.a_character_vector.returns_true_when_string_contains_a_uk_car_licence <- function()
{
  x <- c(
    #1903 to 1931
    "A 1", "AA 9999",                       #ok
    "A 01",                                 #zero prefix on number
    "S0", "G0", "RG0", "LM0",               #ok, special plates
    #1931 to 1963
    "AAA 1", "AAA 999",                     #ok
    "III 1", "QQQ 1", "ZZZ 1",              #disallowed letters
    "AAA 01",                               #zero prefix on number
    #1931 to 1963 alt
    "1 AAA", "9999 AAA",                    #ok
    "1 III", "1 QQQ", "1 ZZZ",              #disallowed letters
    "01 AAA",                               #zero prefix on number
    #1963 to 1982
    "AAA 1A", "AAA 999A",                   #ok
    "AAA 1I", "AAA 1O", "AAA 1Q",           #disallowed letters
    "AAA 1U", "AAA 1Z", 
    "AAA 01A",                              #zero prefix on number
    #1982 to 2001
    "A1 AAA", "A999 AAA",                   #ok    
    "I1 AAA", "O1 AAA",                     #disallowed letters
    "U1 AAA", "Z1 AAA",
    "A01 AAA",                              #zero prefix on number
    #2001 to 2051
    "AA00 AAA", "AA99 AAA",                 #ok
    "II00 AAA", "QQ00 AAA", "ZZ00 AAA",     #disallowed letters
    "AA00 III", "AA00 QQQ"
  )
  expected <- c(
    TRUE, TRUE, 
    FALSE,
    TRUE, TRUE, TRUE, TRUE,
    TRUE, TRUE,
    FALSE, FALSE, FALSE,
    FALSE,
    TRUE, TRUE,
    FALSE, FALSE, FALSE,
    FALSE,
    TRUE, TRUE,
    FALSE, FALSE, FALSE,
    FALSE, FALSE,
    FALSE,
    TRUE, TRUE,
    FALSE, FALSE,
    FALSE, FALSE,
    FALSE,
    TRUE, TRUE,
    FALSE, FALSE, FALSE,
    FALSE, FALSE
  )
  names(expected) <- x
  checkEquals(
    expected,
    is_uk_car_licence(x)
  )
} 


test.is_uk_national_insurance_number.a_character_vector.returns_true_when_string_contains_a_uk_national_insurance_number <- function()
{
  x <- c(
    "AA 00 00 00 A", "AA 00 00 00", "AA000000A",                #ok
    "ZZ 99 99 99 M", "ZZ 99 99 99", "ZZ999999M",                
    "DA 00 00 00", "FA 00 00 00", "IA 00 00 00",                #bad first letter
    "QA 00 00 00", "UA 00 00 00", "VA 00 00 00",
    "AD 00 00 00", "AF 00 00 00", "AI 00 00 00", "AO 00 00 00", #bad second letter
    "AQ 00 00 00", "AU 00 00 00", "AV 00 00 00",
    "AA 00 00 00 E", "AA 00 00 00 G", "AA 00 00 00 H",          #bad final letter
    "AA 00 00 00 I", "AA 00 00 00 J", "AA 00 00 00 K",
    "AA 00 00 00 L", "AA 00 00 00 N", "AA 00 00 00 O",
    "AA 00 00 00 P", "AA 00 00 00 Q", "AA 00 00 00 R",
    "AA 00 00 00 S", "AA 00 00 00 T", "AA 00 00 00 U",
    "AA 00 00 00 V", "AA 00 00 00 W", "AA 00 00 00 X",
    "AA 00 00 00 Y", "AA 00 00 00 Z"    
  )
  expected <- rep(c(TRUE, FALSE), times = c(6, 33))
  names(expected) <- x
  checkEquals(
    expected,
    is_uk_national_insurance_number(x)
  )
} 


test.is_uk_postcode.a_character_vector.returns_true_when_string_contains_a_uk_postcode <- function()
{
  x <- c(
    "SW1A 1AA", "SK11 9DW", "M34FP",  #ok
    "Le45ns", "TS25 2BZ", "gir 0aa",
    "Q1 1AA", "V1 1AA", "X1 1AA",     #bad first letter in area
    "A01 1AA", "A100 1AA", "A1 10AA", #bad numbers
    "A1 1CA", "A1 1IA", "A1 1KA",     #bad letter in district
    "A1 1MA", "A1 1OA", "A1 1VA"
  )
  expected <- rep(c(TRUE, FALSE), times = c(6, 12))
  names(expected) <- x
  checkEquals(
    expected,
    is_uk_postcode(x)
  )
} 


test.is_uk_telephone_number.a_character_vector.returns_true_when_string_contains_a_uk_telephone_number <- function()
{
  x <- c(
    "+44 207 219 3475", "020",   #ok new style city
    "", "",
    "", "",                      #ok standard city
    "", "",
    "01200123456", "01202123456",                      #ok regional (4+6)
    "01204123456", "01205123456",
    "01206123456", "01207123456",
    "01208123456", "01209123456",
    "01223123456", "01224123456",
    "01225123456", "01226123456",
    "01227123456", "01228123456",
    "01229123456", "01233123456",
    "01234123456", "01235123456",
    "01236123456", "01237123456",
    "01239123456", "01241123456",
    "01242123456", "01243123456",
    "01244123456", "01245123456",
    "01246123456", "01248123456",
    "01249123456", "01250123456",
    "01252123456", "01253123456",
    "01254123456", "01255123456",
    "01256123456", "01257123456",
    "01258123456", "01259123456",
    "01280123456", "01282123456",
    "01283123456", "01284123456",
    "01285123456", "01286123456",
    "01287123456", "01288123456",
    "01289123456", "01260123456",
    "01261123456", "01262123456",
    "01263123456", "01264123456",
    "01267123456", "01268123456",
    "01269123456", "01270123456",
    "01271123456", "01273123456",
    "01274123456", "01275123456",
    "01276123456", "01277123456",
    "01278123456", "01279123456",
    "01291234567", "01301234567",
    "01320123456", "01322123456",
    "01323123456", "01324123456",
    "01325123456", "01326123456",
    "01327123456", "01328123456",
    "01329123456", "01350123456",
    "01352123456", "01353123456",
    "01354123456", "01355123456",
    "01356123456", "01357123456",
    "01358123456", "01359123456",
    "01330123456", "01332123456",
    "01333123456", "01334123456",
    "01335123456", "01337123456",
    "01339123456", "01340123456",
    "01341123456", "01342123456",
    "01343123456", "01344123456",
    "01346123456", "01347123456",
    "01348123456", "01349123456",
    "01360123456", "01361123456",
    "01362123456", "01363123456",
    "01364123456", "01366123456",
    "01367123456", "01368123456",
    "01369123456", "01380123456",
    "01381123456", "01382123456",
    "01383123456", "01384123456",
    "01386123456", "01387123456",
    "01388123456", "01389123456",
    "01371123456", "01372123456",
    "01373123456", "01375123456",
    "01376123456", "01377123456",
    "01379123456", "01392123456",
    "01394123456", "01395123456",
    "01397123456", "01398123456",
    "01400123456", "01403123456",
    "01404123456", "01405123456",
    "01406123456", "01407123456",
    "01408123456", "01409123456",
    "01420123456", "01422123456",
    "01423123456", "01424123456",
    "01425123456", "01427123456",
    "01428123456", "01429123456",
    "01431234567", "01471234567",
    "01440123456", "01442123456",
    "01443123456", "01444123456",
    "01445123456", "01446123456",
    "01449123456", "01450123456",
    "01451123456", "01452123456",
    "01453123456", "01454123456",
    "01455123456", "01456123456",
    "01457123456", "01458123456",
    "01460123456", "01461123456",
    "01462123456", "01463123456",
    "01464123456", "01465123456",
    "01466123456", "01467123456",
    "01469123456", "01490123456",
    "01491123456", "01492123456",
    "01493123456", "01494123456",
    "01495123456", "01496123456",
    "01497123456", "01499123456",
    "01501123456", "01502123456",
    "01503123456", "01505123456",
    "01506123456", "01507123456",
    "01508123456", "01509123456",
    "01520123456", "01522123456",
    "01524123456", "01525123456",
    "01526123456", "01527123456",
    "01528123456", "01529123456",
    "01530123456", "01531123456",
    "01534123456", "01535123456",
    "01536123456", "01538123456",
    "01539123456", "01540123456",
    "01542123456", "01543123456",
    "01543123456", "01545123456",
    "01546123456", "01547123456",
    "01548123456", "01549123456",
    "01550123456", "01553123456",
    "01554123456", "01555123456",
    "01556123456", "01557123456",
    "01558123456", "01559123456",
    "01561234567", "01570123456",
    "01571123456", "01572123456",
    "01573123456", "01575123456",
    "01576123456", "01577123456",
    "01578123456", "01579123456",
    "01580123456", "01581123456",
    "01581223456", "01583123456",
    "01581423456", "01586123456",
    "01588123456", "01590123456",
    "01591123456", "01592123456",
    "01593123456", "01594123456",
    "01595123456", "01597123456",
    "01598123456", "01599123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0123456", "0123456",
    "0120461123", "0120462123",                      #ok regional (6+3)
    "0120463123", "0120464123",
    "0120872123", "0120873123",
    "0120874123", "0120875123",
    "0120876123", "0120877123",
    "0120878123", "0120879123",
    "0125451123", "0125452123",
    "0125451323", "0125454123",
    "0125455123", "0125456123",
    "0125457123", "0125459123",
    "0127621234", "0127631123",
    "0127632123", "0127633123",
    "0127634123", "0127635123",
    "0127636123", "0127637123",
    "0127638123", "0127661123",
    "0127661123", "0127661123",
    "0127661123", "0127661123",
    "0127661123", "0129720123",
    "0129721123", "0129722123",
    "0129723123", "0129724123",
    "0129732123", "0129733123",
    "0129734123", "0129735123",
    "0129822123", "0129823123",
    "0129824123", "0129825123",
    "0129826123", "0129827123",
    "0129828123", "0129870123",
    "0129871123", "0129872123",
    "0129873123", "0129874123",
    "0129877123", "0129878123",
    "0129879123", "0129883123",
    "0129884123", "0129885123",
    "0136382123", "0136383123",
    "0136384123", "0136385123",
    "0136472123", "0136473123",
    "0138470123", "0138474123",
    "0138474123", "0138476123",
    "0138477123", "0138478123",
    "0138479123", "0138640123",
    "0138641123", "0138645123",
    "0138647123", "0138648123",
    "0138649123", "0140441123",
    "0140442123", "0140443123",
    "0140444123", "0140445123",
    "0140446123", "0140447123",
    "0142022123", "0142023123",
    "0142081234", "0146030123",
    "0146052123", "0146053123",
    "0146054123", "0146055123",
    "0146057123", "0146061123",
    "0146062123", "0146063123",
    "0146064123", "0146065123",
    "0146066123", "0146067123",
    "0146068123", "0146072123",
    "0146073123", "0146074123",
    "0146075123", "0146076123",
    "0146077123", "0146078123",
    "0146140123", "0148052123",
    "0148871123", "0148872123",
    "0148873123", "0152432123",
    "0152432323", "0152434123",
    "0152432523", "0152436123",
    "0152432723", "0152439123",
    "0152461234", "0152761234",
    "0156260123", "0156266123",
    "0156267123", "0156268123",
    "0156269123", "0156686123",
    "0160641234", "0160674123",
    "0160675123", "0160676123",
    "0160677123", "0160679123",
    "0162955123", "0162956123",
    "0162957123", "0163531234",
    "0163541234", "0164724123",
    "0164761123", "0165950123",
    "0165958123", "0165966123",
    "0165967123", "0165974123",
    "0169550123", "0169551123",
    "0169552123", "0169553123",
    "0169554123", "0172661123",
    "0172663123", "0172664123",
    "0172665123", "0172666123",
    "0172667123", "0172668123",
    "0172669123", "0172670123",
    "0172671123", "0172672123",
    "0172673123", "0172674123",
    "0172675123", "0172676123",
    "0172677123", "0174421234",
    "0175020123", "0175021123",
    "0175022123", "0175023123",
    "0175032123", "0175042123",
    "0175052123", "0175062123",
    "0175082123", "0175076123",
    "0182751234", "0182761234",
    "0183752123", "0183753123",
    "0183754123", "0183755123",
    "0183782123", "0183783123",
    "0183789123", "0188432123",
    "0188433123", "0188434123",
    "0188435123", "0188438123",
    "0190061123", "0190062123",
    "0190063123", "0190064123",
    "0190065123", "0190066123",
    "0190067123", "0190068123",
    "0190085123", "0190521234",
    "0193583123", "0194661123",
    "0194662123", "0194663123",
    "0194664123", "0194665123",
    "0194666123", "0194667123",
    "0194668123", "0194920123",
    "0194921123", "0194981123",
    "0196323123", "0196331123",
    "0196332123", "0196333123",
    "0196334123", "0199561123",
    "0176888212", "0176888412",                      #ok special regional
    "0176888412", "0176888612",
    "0176888712", "0176888812",
    "0169772123", "0169773123",
    "07112345678", "07212345678",                      #ok mobiles
    "07312345678", "07412345678",
    "07112345678", "07511234567",
    "07531234567", "07541234567",
    "07551234567", "07561234567",
    "07571234567", "07581234567",
    "07591234567", "07501123456",
    "07520123456", "07624123456",
    "07711234567", "07801234567",
    "07901234567", 
    "07600123456", "07601123456",                      #ok pagers
    "07602123456", "07623123456",
    "07625123456", "07626123456",
    "07640123456", "07641123456",
    "07643123456", "07644123456",
    "07654123456", "07659123456",
    "07660123456", "07661123456",
    "07662123456", "07663123456",
    "07666123456", "07669123456",
    "07677123456", "07681123456",
    "07693123456", "07699123456",
    "08001111", "0800123456",                      #ok free
    "08001234567", "08081234567", 
    "0500123456",
    "07811234567", "07821234567",                      #ok premium
    "07831234567", "09011234567",
    "09111234567", "09811234567",
    "09821234567", "09831234567",    
    "08421234567", "08431234567",        #ok shared
    "08441234567", "08451234567",
    "08701234567",
    "07012345678",                      #ok personal
    "05612345678",                      #ok VoIP
    "05512345678", "03012345678",                      #ok UAN
    "033123445678", "03412345678", 
    "03712345678",
    "", "",                      #bad country code
    "", "",                      #too many digits
    "", "",                      #not enough digits
    "", "",                      #bad new style city
    "", "",
    "", "",                      #bad standard city
    "", "",
    "", "",                      #bad regional (4+6)    
    "", "",
    "", "",
    "", "",
    "", "",
    "", "",
    "", "",
    "", "",
    "", "",                      #bad regional (6+3)
    "", "",
    "", "",
    "", "",
    "", "",
    "", "",
    "", "",
    "", "",
    "0176888112", "0176888512",                      #bad special regional
    "0176888912", "0169770123",
    "0169771123", "0169774123",
    "0169775123", "0169776123",
    "0169777123", "0169778123",
    "0169779123",
    "07509123456", "07524123456",                      #bad mobiles
    "07700123456", "07781123456",
    "07797123456", "07829123456",                      
    "07831923456", "07911123456",
    "07924123456", "07937123456",                      
    "07611234567", "07611234567",                      #bad pagers
    "07631234567", "",                               
    "08741234567", "08751234567",                      #bad premium
    "08761234567", "08771234567",
    "08781234567", "08791234567",
    "09212345678", "09312345678",
    "09212345678", "09312345678",
    "09412345678", "09512345678",
    "09612345678", "09712345678",
    "09912345678", "09801234567",
    "09841234567", "09851234567",
    "09861234567", "09871234567",
    "09881234567", "09891234567",
    "08401234567", "08411234567",                      #bad shared
    "08461234567", "08471234567",
    "08481234567", "08491234567",
    "08410234567", "08411234567",                      #bad personal
    "08460234567", "08470234567",                     
    "08480234567", "08490234567",                     
    "03112345678", "03212345678",                      #bad UAN
    "03812345678", "03912345678"
  )
  expected <- rep(c(TRUE, FALSE), times = c(6, 12))
  names(expected) <- x
  checkEquals(
    expected,
    is_uk_postcode(x)
  )
} 


test.is_valid_r_code.valid_r_code.returns_true <- function()
{
  x <- "x <- 1 + sqrt(pi); y <- sin(x)"
  checkTrue(is_valid_r_code(x))
}

test.is_valid_r_code.invalid_r_code.returns_false <- function()
{
  x <- "x <- 1 + sqrt(pi) y <- sin(x)"
  checkTrue(!is_valid_r_code(x))  
}


test.is_valid_variable_name.a_character_vector.returns_true_when_string_contains_a_valid_variable_name <- function()
{
  x <- c(
    "x", "Y1", "zZ._..1",                      #ok
    ".", "..", "....",
    "1x", ".1x",                               #starts with number
    "_", "_x",                                 #starts with underscore
    paste0(rep.int("x", 10001), collapse = "") #too long
  )
  expected <- rep(c(TRUE, FALSE), times = c(6, 5))
  names(expected) <- x
  checkEquals(
    expected,
    is_valid_variable_name(x)
  )
} 

test.is_valid_variable_name.reserved_names.returns_true_when_allow_reserved_is_true <- function()
{
  x <- c(
    "...", "..1", "..0", "..999999999"     
  )
  expected <- rep.int(TRUE, length(x))
  names(expected) <- x
  checkEquals(
    expected,
    is_valid_variable_name(x)
  )
  checkEquals(
    !expected,
    is_valid_variable_name(x, allow_reserved = FALSE)
  )
} 

test.is_valid_variable_name.repeated_names.returns_true_when_allow_duplicates_is_true <- function()
{
  x <- rep.int("x", 2)
  expected <- rep.int(TRUE, length(x))
  names(expected) <- x
  checkEquals(
    expected,
    is_valid_variable_name(x)
  )
  expected[2] <- FALSE
  checkEquals(
    expected,
    is_valid_variable_name(x, allow_duplicates = FALSE)
  )
} 
