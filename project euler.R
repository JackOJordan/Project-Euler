cat("\014")

# Problem 1 - Multiples of 3 or 5 - SOLVED

nums <- 1:999
sum(nums[(nums %% 3) == 0 | (nums %% 5) == 0])

# Problem 2 - Even Fibonacci numbers - SOLVED

nums <- c(1, 2)
for(i in 1:4000000){
  nums[i + 2] <- nums[i + 1] + nums[i]
  if(nums[i + 2] >= 4000000) nums[i + 2] <- 0 & break
}
sum(nums[(nums %% 2) == 0])

# Problem 3 - Largest prime factor - SOLVED

testNum <- 600851475143
nums <- 2:10000
for(i in 1:testNum){
  nums <- nums[nums == nums[i] | !((nums %% nums[i]) == 0)]
  if(i == length(nums)) break
}
primes <- c()
for (i in 1:length(nums)){
  if(testNum %% nums[i] == 0) primes[i] <- nums[i]
}
primes <- primes[!is.na(primes)]
max(primes)

# Problem 4 - Largest palindrome product - SOLVED* (FAIL - Forgot palindromes containing 0s)

palindromes <- c()
for(g in 0:9){
  for(h in 0:9){
    for(i in 0:9){
      palindromes[1 + 10^0 * (i - 1) + 10^1 * (h - 1) + 10^2 * (g - 1)] <- g*100001 + h*10010 + i*1100
    }
  }
}
palindromes <- sort(palindromes, decreasing = TRUE)
nums <- 100:999
for(i in 1:1000){
  test <- palindromes[i]
  factors <- nums[test %% nums == 0]
  secondFactor <- test/factors
  secondFactor <- secondFactor[secondFactor %in% nums]
  if(length(secondFactor) > 0) {print(c(test, max(factors), min(secondFactor))); break}
}

# Problem 5 - Smallest multiple - SOLVED

primesUnder <- function(x){
  nums <- 2:x
  primes <- c()
  for(i in 1:length(nums)){
    lessThan <- nums[nums < nums[i]]
    remainders <- nums[i] %% lessThan
    if(!(0 %in% remainders)) primes[i] <- nums[i]
  }
  primes <- primes[!(is.na(primes))]
  primes
}
primeDecomp <- function(x){
  # ifelse(x == 1, primeFactors <- 1,
  {primes <- primesUnder(x)
  product <- c(1)
  remaining <- x / product
  primeFactors <- c()
  while (remaining > 1) {
    primeFactors <- append(primeFactors, primes[remaining %% primes == 0])
    for(i in 1:length(primeFactors)){
      product[1] <- product[1]*primeFactors[i]
    }
    remaining <- x / product
    product <- c(1)
  }
  sort(primeFactors, decreasing = TRUE)
  }
  # )
}
product <- function(x){
  productVec <- c(1)
  if (length(x) > 0) {
    for(i in 1:length(x)){
    productVec[1] <- productVec[1]*x[i]
    }
  }
  productVec[1]
}
factors <- c()
workingFactors <- c()
for(i in 2:20){
  workingFactors <- c(primeDecomp(i))
  for(j in 1:length(workingFactors)){
    if(is.element(workingFactors[j],factors) == TRUE)
    { 
      factors[match(workingFactors[j], factors)] <- NA
    }
  }
  factors <- c(factors, workingFactors)
  factors <- factors[!(is.na(factors))]
}
product(factors)

# Problem 6 - Sum square difference - SOLVED

sum(1:100)^2 - sum((1:100)^2)

# Problem 7 - 10001st prime - SOLVED

nums <- 2:200000
for(i in 1:length(nums)){
  nums <- nums[nums == nums[i] | !(nums %% nums[i]) == 0]
  if(i == length(nums)) break
}
length(nums)
nums[10001]

# Problem 8 - Largest product in a series - SOLVED

test <- as.character("7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")
test <- strsplit(test, split = "")
number <- 13
mat <- matrix(data = NA, nrow = length(test[[1]]) - number + 1, ncol = number)
for(i in 1:(length(test[[1]]) - number + 1)){
   for(j in 1:number){
     mat[i,j] <- as.numeric(test[[1]][i + j - 1])
   }
}
product <- function(x){
   workingProduct <- c(1)
   for(i in 1:length(x)){
       workingProduct[1] <- workingProduct[1]*x[i]
     }
   print(workingProduct)
}
maxProduct <- c(1)
for(i in 1:(length(test[[1]]) - number + 1)){
   workingRowProduct <- product(mat[i,])
   if(workingRowProduct > maxProduct) maxProduct <- workingRowProduct
}
maxProduct

# Problem 9 - Special Pythagorean triplet - SOLVED

a <- 1:300
b <- 1:999
c <- (1:999)^2
k <- c()
for(i in a){
   for(j in b){
       k <- i^2 + j^2
       if(!(k %in% c)) {k <- NA; next}
       l <- sum(i, j, sqrt(k))
       if (l == 1000) {print(c(i, j, sqrt(k))); print(i*j*sqrt(k)); break}
     }
}

# Problem 10 - Summation of primes - SOLVED (function doesn't break properly)

a <- seq(3, 2000000, by = 2)
for(i in 1:length(a)){
   if(a[i] > 1000000) break
   a <- a[a == a[i] | !((a %% a[i]) == 0)]
}
sum(a) + 2

# Problem 11 - Largest product in a grid - SOLVED

grid <- c(08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08,
49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00,
81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65,
52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91,
22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80,
24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50,
32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70,
67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21,
24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72,
21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95,
78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92,
16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57,
86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58,
19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40,
04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66,
88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69,
04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36,
20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16,
20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54,
01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48)
rows <- sqrt(length(grid))
number <- 4
mat <- matrix(grid, nrow = rows, ncol = rows, byrow = TRUE)
maxProduct <- c(1)
nums <- c("",1,1,1,1,1,1)
for(i in 1:rows){
  for(j in 1:(rows - number + 1)){
    product <- c(1)
    product[1] <- mat[i,j]*mat[i,j+1]*mat[i,j+2]*mat[i,j+3]
    if (product[1] > maxProduct) {maxProduct <- product[1]; nums <- c("rows",i,j,mat[i,j],mat[i,j+1],mat[i,j+2],mat[i,j+3])}
  }
}
for(i in 1:(rows - number + 1)){
  for(j in 1:rows){
    product <- c(1)
    product[1] <- mat[i,j]*mat[i+1,j]*mat[i+2,j]*mat[i+3,j]
    if (product[1] > maxProduct) {maxProduct <- product[1]; nums <- c("columns",i,j,mat[i,j],mat[i+1,j],mat[i+2,j],mat[i+3,j])}
  }
}
for(i in 1:(rows - number + 1)){
  for(j in 1:(rows - number + 1)){
    product <- c(1)
    product[1] <- mat[i,j]*mat[i+1,j+1]*mat[i+2,j+2]*mat[i+3,j+3]
    if (product[1] > maxProduct) {maxProduct <- product[1]; nums <- c("down-diag",i,j,mat[i,j],mat[i+1,j+1],mat[i+2,j+2],mat[i+3,j+3])}
  }
}
for(i in 4:rows){
  for(j in 1:(rows - number + 1)){
    product <- c()
    product[1] <- mat[i,j]*mat[i-1,j+1]*mat[i-2,j+2]*mat[i-3,j+3]
    if (product[1] > maxProduct) {maxProduct <- product[1]; nums <- c("up-diag",i,j,mat[i,j],mat[i-1,j+1],mat[i-2,j+2],mat[i-3,j+3])}
  }
}
maxProduct
nums

# Problem 12 - Highly divisible triangular number - SOLVED

nums <- 1:50000
triangles <- c()
for(i in nums){
  triangles[i] = sum(nums[nums <= nums[i]])
}
divisors <- function(x){
  lessThan <- 1:sqrt(x)
  length(lessThan[(x %% lessThan) == 0])
}
maxDivs <- c(1)
for(i in nums){
  test <- divisors(triangles[i])
  if (test > maxDivs) maxDivs <- test
  if (maxDivs > 250) {print(c(i,triangles[i],maxDivs)); break}
  if (i == length(nums)) print(c(i,triangles[i],maxDivs))
}

# Problem 13 - Large sum - SOLVED

sum(37107287533902102798797998220837590246510135740250,
46376937677490009712648124896970078050417018260538,
74324986199524741059474233309513058123726617309629,
91942213363574161572522430563301811072406154908250,
23067588207539346171171980310421047513778063246676,
89261670696623633820136378418383684178734361726757,
28112879812849979408065481931592621691275889832738,
44274228917432520321923589422876796487670272189318,
47451445736001306439091167216856844588711603153276,
70386486105843025439939619828917593665686757934951,
62176457141856560629502157223196586755079324193331,
64906352462741904929101432445813822663347944758178,
92575867718337217661963751590579239728245598838407,
58203565325359399008402633568948830189458628227828,
80181199384826282014278194139940567587151170094390,
35398664372827112653829987240784473053190104293586,
86515506006295864861532075273371959191420517255829,
71693888707715466499115593487603532921714970056938,
54370070576826684624621495650076471787294438377604,
53282654108756828443191190634694037855217779295145,
36123272525000296071075082563815656710885258350721,
45876576172410976447339110607218265236877223636045,
17423706905851860660448207621209813287860733969412,
81142660418086830619328460811191061556940512689692,
51934325451728388641918047049293215058642563049483,
62467221648435076201727918039944693004732956340691,
15732444386908125794514089057706229429197107928209,
55037687525678773091862540744969844508330393682126,
18336384825330154686196124348767681297534375946515,
80386287592878490201521685554828717201219257766954,
78182833757993103614740356856449095527097864797581,
16726320100436897842553539920931837441497806860984,
48403098129077791799088218795327364475675590848030,
87086987551392711854517078544161852424320693150332,
59959406895756536782107074926966537676326235447210,
69793950679652694742597709739166693763042633987085,
41052684708299085211399427365734116182760315001271,
65378607361501080857009149939512557028198746004375,
35829035317434717326932123578154982629742552737307,
94953759765105305946966067683156574377167401875275,
88902802571733229619176668713819931811048770190271,
25267680276078003013678680992525463401061632866526,
36270218540497705585629946580636237993140746255962,
24074486908231174977792365466257246923322810917141,
91430288197103288597806669760892938638285025333403,
34413065578016127815921815005561868836468420090470,
23053081172816430487623791969842487255036638784583,
11487696932154902810424020138335124462181441773470,
63783299490636259666498587618221225225512486764533,
67720186971698544312419572409913959008952310058822,
95548255300263520781532296796249481641953868218774,
76085327132285723110424803456124867697064507995236,
37774242535411291684276865538926205024910326572967,
23701913275725675285653248258265463092207058596522,
29798860272258331913126375147341994889534765745501,
18495701454879288984856827726077713721403798879715,
38298203783031473527721580348144513491373226651381,
34829543829199918180278916522431027392251122869539,
40957953066405232632538044100059654939159879593635,
29746152185502371307642255121183693803580388584903,
41698116222072977186158236678424689157993532961922,
62467957194401269043877107275048102390895523597457,
23189706772547915061505504953922979530901129967519,
86188088225875314529584099251203829009407770775672,
11306739708304724483816533873502340845647058077308,
82959174767140363198008187129011875491310547126581,
97623331044818386269515456334926366572897563400500,
42846280183517070527831839425882145521227251250327,
55121603546981200581762165212827652751691296897789,
32238195734329339946437501907836945765883352399886,
75506164965184775180738168837861091527357929701337,
62177842752192623401942399639168044983993173312731,
32924185707147349566916674687634660915035914677504,
99518671430235219628894890102423325116913619626622,
73267460800591547471830798392868535206946944540724,
76841822524674417161514036427982273348055556214818,
97142617910342598647204516893989422179826088076852,
87783646182799346313767754307809363333018982642090,
10848802521674670883215120185883543223812876952786,
71329612474782464538636993009049310363619763878039,
62184073572399794223406235393808339651327408011116,
66627891981488087797941876876144230030984490851411,
60661826293682836764744779239180335110989069790714,
85786944089552990653640447425576083659976645795096,
66024396409905389607120198219976047599490197230297,
64913982680032973156037120041377903785566085089252,
16730939319872750275468906903707539413042652315011,
94809377245048795150954100921645863754710598436791,
78639167021187492431995700641917969777599028300699,
15368713711936614952811305876380278410754449733078,
40789923115535562561142322423255033685442488917353,
44889911501440648020369068063960672322193204149535,
41503128880339536053299340368006977710650566631954,
81234880673210146739058568557934581403627822703280,
82616570773948327592232845941706525094512325230608,
22918802058777319719839450180888072429661980811197,
77158542502016545090413245809786882778948721859617,
72107838435069186155435662884062257473692284509516,
20849603980134001723930671666823555245252804609722,
53503534226472524250874054075591789781264330331690)/10^42

# Problem 14 - Largest Collatz sequence - SOLVED

unwantedEvens <- seq(500002,1000000,by = 6)
allEvens <- seq(500002,1000000,by = 2)
wantedEvens <- allEvens[!(allEvens %in% intersect(unwantedEvens, allEvens))]
nums <- c(seq(500001,999999, by = 2),wantedEvens)
maxLength <- c(1,1)
for(j in nums){
  num <- c(j)
  for(i in 1:100000){
    if(num[i] == 1) {maxLength[2] <- max(maxLength[2], length(num))}
    if(maxLength[2] == length(num)) maxLength[1] <- num[1]
    if(num[i] == 1) break
    if(num[i] %% 2 == 0) num[i+1] <- num[i]/2
    else num[i+1] <- 3*num[i]+1
  }
}
maxLength

# Problem 15 - Lattice paths - SOLVED

mat <- matrix(nrow = 21, ncol = 21)
mat[1,] <- mat[,1] <- 1
for(i in 2:(sqrt(length(mat)))){
  for(j in 2:(sqrt(length(mat)))){
    mat[i,j] <- mat[i-1,j]+mat[i,j-1]
  }
}
mat[21,21]

# Problem 16 - Power digit sum - SOLVED (2^1000 in R is wrong. Thanks Wolfram for the real number.)

# vec <- 2^1000
# nums <- c()
# for(i in 1:(expo+1)){
#   nums[i] <- floor((vec - floor(vec/10^(expo+1-i+1)))/10^(expo+1-i))
#   vec <- vec - floor(vec/10^(expo+1-i))*10^(expo+1-i)
#   print(vec)
# }
# sum(nums)

vec <- strsplit("10715086071862673209484250490600018105614048117055336074437503883703510511249361224931983788156958581275946729175531468251871452856923140435984577574698574803934567774824230985421074605062371141877954182153046474983581941267398767559165543946077062914571196477686542167660429831652624386837205668069376", split = "")
sum(as.numeric(vec[[1]]))

# Problem 17 - Number letter counts - SOLVED

words <- c("one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety","hundred","thousand","and")
number <- 0
number <- number + 191*length(strsplit(words[1], split = "")[[1]])
for(i in 2:9){
  number <- number + 190*length(strsplit(words[i], split = "")[[1]])
}
for(i in 10:19){
  number <- number + 10*length(strsplit(words[i], split = "")[[1]])
}
for(i in 20:27){
  number <- number + 100*length(strsplit(words[i], split = "")[[1]])
}
number <- number + 900*length(strsplit(words[28], split = "")[[1]])
number <- number + length(strsplit(words[29], split = "")[[1]])
number <- number + 891*length(strsplit(words[30], split = "")[[1]])
number

# Problem 18 - Maximum path sum I - SOLVED

vec <- c(
  75,
  95, 64,
  17, 47, 82,
  18, 35, 87, 10,
  20, 04, 82, 47, 65,
  19, 01, 23, 75, 03, 34,
  88, 02, 77, 73, 07, 63, 67,
  99, 65, 04, 28, 06, 16, 70, 92,
  41, 41, 26, 56, 83, 40, 80, 70, 33,
  41, 48, 72, 33, 47, 32, 37, 16, 94, 29,
  53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14,
  70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57,
  91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48,
  63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31,
  04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23
  )
mat <- matrix(NA, nrow = 15, ncol = 15)
triangles <- c()
for(i in 1:sqrt(length(mat))){
  triangles[i] <- sum(1:i)
}
mat[1,1] <- vec[1]
for(i in 2:sqrt(length(mat))){
  for(j in 1:sqrt(length(mat))){
    mat[i,j] <- vec[triangles[i - 1] + j]
    if(j == i) break
  }
}
mat
for(i in 14:1){
  for(j in 14:1){
    mat[i,j] <- mat[i,j] + max(mat[i+1,j],mat[i+1,j+1])
  }
}
mat[1,1]

# Problem 19 - Counting Sundays - SOLVED

require(lubridate)
startDate <- ymd("1901,1,1")
days <- wday(startDate %m+% months(1:1200))
length(days[days == 7])

# Problem 20 - Factorial digit sum - SOLVED (Thanks Wolfram, again.)

bigNum <- strsplit("93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000", split = "")
sum <- 0
for(i in 1:length(bigNum[[1]])){
  sum <- sum + as.numeric(bigNum[[1]][i])
}
sum

# Problem 21 - Amicable numbers - SOLVED

nums <- 1:10000
propDivsSum <- function(x){
  lessThan <- nums[nums <= sqrt(x)]
  smallDivs <- lessThan[x %% lessThan == 0]
  divs <- c(smallDivs,x/smallDivs)
  divs <- sort(divs[!duplicated(divs)])
  c(x, sum(divs[-length(divs)]))
}
mat1 <- matrix(NA, nrow = length(nums), ncol = 2)
for(i in nums){
  mat1[i,] <- propDivsSum(i)
}
for(j in 1:50){
  for(i in 1:(length(mat1)/2)){
    if(!(mat1[i,2] %in% mat1[,1])) mat1[i,] <- NA
    if(!(mat1[i,1] %in% mat1[,2])) mat1[i,] <- NA
    if(mat1[i,1] %in% mat1[i,2]) mat1[i,] <- NA
  }
  if(length(mat1[complete.cases(mat1) == TRUE,]) == length(mat1)) {mat1 <- mat1[complete.cases(mat1) == TRUE,]; break}
  mat1 <- mat1[complete.cases(mat1) == TRUE,]
}
sum(mat1)/2
mat1

# Problem 22 - Names scores - SOLVED

namesConnect <- file(description = "p022_names.txt", open = "r", blocking = TRUE)
names <- readLines(namesConnect)
names <- strsplit(names, split = "\",\"")
names[[1]][1] <- "MARY"; names[[1]][length(names[[1]])] <- "ALONSO"
maxi <- c(0)
len <- c()
for(i in 1:length(names[[1]])){
  len <- length(strsplit(names[[1]][i], split = "")[[1]])
  maxi <- max(maxi,len)
}
mat <- matrix(NA, nrow = length(names[[1]]), ncol = 2 + maxi)
df <- as.data.frame(mat)
vec <- c()
for(i in 1:length(names[[1]])){
  vec[i] <- names[[1]][i]
}
vec <- sort(vec)
for(i in 1:length(vec)){
  df[i,1] <- vec[i]
}
for(i in 1:length(df[,1])){
  for(j in 2:(1+maxi)){
    df[i,j] <- strsplit(df[i,1], split = "")[[1]][j-1]
  }
}
for(i in 1:length(df[,1])){
  for(j in 2:(1+maxi)){
    if(is.na(df[i,j]) == TRUE) df[i,j] <- 0
    if((df[i,j] == "A") == TRUE) df[i,j] <- 1
    if((df[i,j] == "B") == TRUE) df[i,j] <- 2
    if((df[i,j] == "C") == TRUE) df[i,j] <- 3
    if((df[i,j] == "D") == TRUE) df[i,j] <- 4
    if((df[i,j] == "E") == TRUE) df[i,j] <- 5
    if((df[i,j] == "F") == TRUE) df[i,j] <- 6
    if((df[i,j] == "G") == TRUE) df[i,j] <- 7
    if((df[i,j] == "H") == TRUE) df[i,j] <- 8
    if((df[i,j] == "I") == TRUE) df[i,j] <- 9
    if((df[i,j] == "J") == TRUE) df[i,j] <- 10
    if((df[i,j] == "K") == TRUE) df[i,j] <- 11
    if((df[i,j] == "L") == TRUE) df[i,j] <- 12
    if((df[i,j] == "M") == TRUE) df[i,j] <- 13
    if((df[i,j] == "N") == TRUE) df[i,j] <- 14
    if((df[i,j] == "O") == TRUE) df[i,j] <- 15
    if((df[i,j] == "P") == TRUE) df[i,j] <- 16
    if((df[i,j] == "Q") == TRUE) df[i,j] <- 17
    if((df[i,j] == "R") == TRUE) df[i,j] <- 18
    if((df[i,j] == "S") == TRUE) df[i,j] <- 19
    if((df[i,j] == "T") == TRUE) df[i,j] <- 20
    if((df[i,j] == "U") == TRUE) df[i,j] <- 21
    if((df[i,j] == "V") == TRUE) df[i,j] <- 22
    if((df[i,j] == "W") == TRUE) df[i,j] <- 23
    if((df[i,j] == "X") == TRUE) df[i,j] <- 24
    if((df[i,j] == "Y") == TRUE) df[i,j] <- 25
    if((df[i,j] == "Z") == TRUE) df[i,j] <- 26
  }
}
for(i in 1:length(df[,1])){
  df[i,(2+maxi)] <- sum(as.numeric(df[2,2]), as.numeric(df[2,3]))
}
mat2 <- matrix(NA, nrow = length(df[,1]), ncol = maxi)
for(i in 1:length(df[,1])){
  for(j in 1:(maxi)){
    mat2[i,j] <- df[i,j+1]
  }
}
nums <- c()
for(i in 1:length(mat2[,1])){
  nums[i] <- sum(as.numeric(mat2[i,]))*i
}
sum(nums)

# Problem 23 - Non-abundant sums - SOLVED

nums <- 1:28123
propDivsSum <- function(x){
  lessThan <- nums[nums <= sqrt(x)]
  divs <- lessThan[x %% lessThan == 0]
  divs <- c(divs, x/divs)
  divs <- divs[!duplicated(divs) & !(divs == x)]
  sum(divs)
}
abundNums <- c()
for(i in nums){
  if(i < propDivsSum(i)) abundNums <- c(abundNums, i)
}
mat <- matrix(NA, nrow = length(abundNums), ncol = length(abundNums))
for(i in 1:length(abundNums)){
  for(j in 1:length(abundNums)){
    mat[i,j] <- abundNums[i] + abundNums[j]
  }
}
vec <- c()
for(i in 1:length(abundNums)){
  for(j in 1:length(abundNums)){
    vec[length(abundNums)*(i-1) + j] <- mat[i,j]
  }
}
vec <- vec[!duplicated(vec) & vec <= length(nums)]
sum(nums) - sum(vec)

# Problem 24 - Lexicographic permutations - SOLVED

els <- c(0,1,2,3,4,5,6,7,8,9)
perm <- c()
position <- 1000000
numLength <- factorial(length(els))/length(els)
whichPerm <- ceiling(position/numLength)
perm[1] <- els[whichPerm]
els <- els[!(els == els[whichPerm])]
for(i in 2:(length(els)+1)){
  position <- position-(whichPerm - 1)*numLength
  numLength <- numLength/(11-i)
  whichPerm <- ceiling(position/numLength)
  perm[i] <- els[whichPerm]
  els <- els[!(els == els[whichPerm])]
}
perm

# Problem 25 - 1000-digit Fibonacci number - SOLVED

num <- 1500
fibNums <- c(1,1)
digits <- c(1,1)
for(i in 3:num){
  fibNums[i] <- fibNums[i-1] + fibNums[i-2]
  digits[i] <- ceiling(log(fibNums[i], base = 10))
}
table(digits)
mean(digits[digits < Inf])
table(table(digits))
goldenRatio <- fibNums[1300]/fibNums[1299]
log(10^99.9, base = goldenRatio)*10 + 2

# Problem 26 - Reciprocal cycles - SOLVED

primesUnder <- function(x){
  nums <- 2:x
  for(i in 1:(x-1)){
    nums <- nums[nums == nums[i] | !(nums %% nums[i] == 0)]
    if(length(nums) == i+1) break
  }
  nums
}
primes <- primesUnder(1000)
primes <- primes[!(primes == 2 | primes == 5)]
orders <- c()
for(i in primes){
  test <- 1
  for(j in 1:1000){
    test <- (10*test) %% i
    if(test == 1) {orders <- c(orders,j); break}
    if(j == i - 1) {orders <- c(orders,j); break}
  }
}
max(orders) + 1

# Problem 27 - Quadratic primes - SOLVED

a <- 1:1000; b <- 1:1000
primesUnder <- function(x){
  nums <- 2:x
  for(i in 1:(x-1)){
    nums <- nums[nums == nums[i] | !(nums %% nums[i] == 0)]
    if(length(nums) == i) break
  }
  nums
}
b <- primesUnder(1000)
b <- sort(c(b,-b))
maxi <- c(1)
for(i in -1000:1000){
  for(n in 0:100){
    if((n^2 + i*n + 2) %in% primes == FALSE) {maxi <- max(maxi,n); break}
  }
}
maxi
a <- a[!(a %% 2 == 0)]
a <- sort(c(a,-a))
best <- c(1)
for(i in a){
  for(j in b){
    for(n in 0:100)
      if((n^2 + i*n + j) %in% primes == FALSE) {best <- max(best,n); break}
  }
}
best
best2 <- c(1)
for(i in a){
  for(j in b){
    for(n in 0:100){
      if((n^2 + i*n + j) %in% primes == FALSE) {best2 <- max(best2,n); break}
    }
    if(best2 == best) break
  }
  if(best2 == best) {print(c(i,j,i*j)); break}
}
vec <- c()
for(n in 0:72){
  vec <- c(vec, n^2 - 61*n + 971)
}
vec
vec %in% primes
length(vec)

# Problem 28 - Number spiral diagonals - SOLVED

# Grid size = n*n, Number of spirals = (n - 1)/2, Final number = n^2

n <- 1001
numSum <- 1
for(i in 1:((n-1)/2)){
  for(j in 1:4){
    numSum <- numSum + (2*i)*j + (2*(i-1)+1)^2
  }
}
numSum

# Problem 29 - Distinct powers - SOLVED

nums <- c()
for(i in 2:100){
  for(j in 2:100){
    nums <- c(nums, j^i)
  }
}
nums <- nums[!duplicated(nums)]
length(nums)

# Problem 30 - Digit fifth powers - SOLVED

upper <- 6*9^5
nums <- 10:upper
wanted <- c()
for(i in nums){
  test <- as.numeric(unlist(strsplit(as.character(i), split = "")))
  if(sum(test^5) == i) wanted <- c(wanted,i)
}
sum(wanted)
wanted

# Problem 31 - Coin sums - SOLVED

only5s2s1s <- c(4)
for(i in 1:(200/5-1)){
  only5s2s1s <- c(only5s2s1s, only5s2s1s[i]+floor(5*(i+1)/2+1))
}
only5s2s1s
only10s5s2s1s <- c(1+only5s2s1s[2])
for(i in 1:(200/10-1)){
  only10s5s2s1s <- c(only10s5s2s1s, only10s5s2s1s[i]+only5s2s1s[2+2*i])
}
only10s5s2s1s
only20s10s5s2s1s <- c(1+only10s5s2s1s[2])
for(i in 1:(200/20-1)){
  only20s10s5s2s1s <- c(only20s10s5s2s1s, only20s10s5s2s1s[i]+only10s5s2s1s[2+2*i])
}
only20s10s5s2s1s
only50s20s10s5s2s1s <- c()
only50s20s10s5s2s1s[1] <- 1+only10s5s2s1s[1]+only10s5s2s1s[3]+only10s5s2s1s[5]
only50s20s10s5s2s1s[2] <- only50s20s10s5s2s1s[1] + only20s10s5s2s1s[5]
only50s20s10s5s2s1s[3] <- only50s20s10s5s2s1s[2] + sum(only10s5s2s1s[seq(1,15,by=2)])
only50s20s10s5s2s1s[4] <- only50s20s10s5s2s1s[3] + only20s10s5s2s1s[10]
only50s20s10s5s2s1s
only100s50s20s10s5s2s1s <- c()
only100s50s20s10s5s2s1s[1] <- 1+only50s20s10s5s2s1s[2]
only100s50s20s10s5s2s1s[2] <- only100s50s20s10s5s2s1s[1]+only50s20s10s5s2s1s[4]
only100s50s20s10s5s2s1s
only200s100s50s20s10s5s2s1s <- c(1+only100s50s20s10s5s2s1s[2])
only200s100s50s20s10s5s2s1s

# Problem 32 - Pandigital products - SOLVED

winner <- 123456789
winProds <- c()
forInterest <- c()

# 3-digit x 2-digit

for(x in 123:786){
  for(y in 12:79){
    z <- x*y
    if (z > 9876) break
    test <- paste(x,y,z, sep = "")
    digits <- sort(unlist(strsplit(as.character(test), split = "")))
    if (TRUE %in% duplicated(digits)) next
    if (0 %in% digits) next
    digits <- as.integer(paste(digits, collapse = ""))
    if (digits == winner) winProds <- c(winProds, z)
    if (digits == winner) forInterest <- c(forInterest, paste(x,y,z, sep = " "))
  }
}

# 4-digit x 1-digit

for(x in 1234:4873){
  for(y in 2:7){ # up to 79
    z <- x*y
    if (z > 9876) break
    test <- paste(x,y,z, sep = "")
    digits <- sort(unlist(strsplit(as.character(test), split = "")))
    if (TRUE %in% duplicated(digits)) next
    if (0 %in% digits) next
    digits <- as.integer(paste(digits, collapse = ""))
    if (digits == winner) winProds <- c(winProds, z)
    if (digits == winner) forInterest <- c(forInterest, paste(x,y,z, sep = " "))
  }
}

winProds <- winProds[!duplicated(winProds)]
sum(winProds)

# Problem 33 - Digit cancelling fractions - SOLVED

numers <- 1
denoms <- 1
forInterest <- c()

for(x in 11:99){
  if (x %% 10 == 0) next
  for(y in 11:99){
    if (y %% 10 == 0) next
    if (x >= y) next
    z <- x/y
    test <- unlist(strsplit(as.character(x), split = ""))
    test2 <- unlist(strsplit(as.character(y), split = ""))
    test3 <- test[!(test %in% test2)]
    if (!(length(test3) == 1)) next
    test4 <- test2[!(test2 %in% test)]
    if (!(length(test4) == 1)) next
    if (z == as.integer(test3)/as.integer(test4)) {numers <- numers*x; denoms <- denoms*y; forInterest <- c(forInterest, paste(x,y,test3,test4, sep = ""))}
  }
}

1/(numers/denoms)

# Problem 34 - Digit factorials - SOLVED

answer <- 0

for(i in 10:2540160){
  if(sum(factorial(as.integer(unlist(strsplit(as.character(i), split = ""))))) == i) answer <- answer + i
}

answer

# Problem 35 - Circular primes - SOLVED

nums <- c(1,3,7,9)
possible6s <- c()
possible5s <- c()
possible4s <- c()
possible3s <- c()

# 6-digits

for(a in 1:length(nums)){
  for(b in 1:length(nums)){
    for(c in 1:length(nums)){
      for(d in 1:length(nums)){
        for(e in 1:length(nums)){
          for(f in 1:length(nums)){
            possible6s <- c(possible6s, as.integer(paste(nums[a],nums[b],nums[c],nums[d],nums[e],nums[f], sep = "")))
          }
        }
      }
    }
  }
}

# 5-digits

for(a in 1:length(nums)){
  for(b in 1:length(nums)){
    for(c in 1:length(nums)){
      for(d in 1:length(nums)){
        for(e in 1:length(nums)){
            possible5s <- c(possible5s, as.integer(paste(nums[a],nums[b],nums[c],nums[d],nums[e], sep = "")))
        }
      }
    }
  }
}

# 4-digits

for(a in 1:length(nums)){
  for(b in 1:length(nums)){
    for(c in 1:length(nums)){
      for(d in 1:length(nums)){
          possible4s <- c(possible4s, as.integer(paste(nums[a],nums[b],nums[c],nums[d], sep = "")))
      }
    }
  }
}

# 3-digits

possible3s <- c()
for(a in 1:length(nums)){
  for(b in 1:length(nums)){
    for(c in 1:length(nums)){
        possible3s <- c(possible3s, as.integer(paste(nums[a],nums[b],nums[c], sep = "")))
    }
  }
}

length(possible3s)
head(possible3s,20)


nums <- 2:1000000
for(i in 1:length(nums)){
  nums <- nums[nums == nums[i] | !(nums %% nums[i]) == 0]
  if(nums[i] > sqrt(max(nums))) break
}

wantedPrimes <- c(possible3s[possible3s %in% nums], possible4s[possible4s %in% nums], possible5s[possible5s %in% nums], possible6s[possible6s %in% nums])

winners <- c()
for(i in 1:length(wantedPrimes)){
  digits <- as.integer(unlist(strsplit(as.character(wantedPrimes[i]), split = "")))
  if (length(digits) == 3 &
      as.integer(paste(digits[2],digits[3],digits[1], sep = "")) %in% wantedPrimes &
      as.integer(paste(digits[3],digits[1],digits[2], sep = "")) %in% wantedPrimes
      ) {winners <- c(winners, wantedPrimes[i]); next}
  if (length(digits) == 4 &
      as.integer(paste(digits[2],digits[3],digits[4],digits[1], sep = "")) %in% wantedPrimes &
      as.integer(paste(digits[3],digits[4],digits[1],digits[2], sep = "")) %in% wantedPrimes &
      as.integer(paste(digits[4],digits[1],digits[2],digits[3], sep = "")) %in% wantedPrimes
      ) {winners <- c(winners, wantedPrimes[i]); next}
  if (length(digits) == 5 &
      as.integer(paste(digits[2],digits[3],digits[4],digits[5],digits[1], sep = "")) %in% wantedPrimes &
      as.integer(paste(digits[3],digits[4],digits[5],digits[1],digits[2], sep = "")) %in% wantedPrimes &
      as.integer(paste(digits[4],digits[5],digits[1],digits[2],digits[3], sep = "")) %in% wantedPrimes &
      as.integer(paste(digits[5],digits[1],digits[2],digits[3],digits[4], sep = "")) %in% wantedPrimes
      ) {winners <- c(winners, wantedPrimes[i]); next}
  if (length(digits) == 6 &
      as.integer(paste(digits[2],digits[3],digits[4],digits[5],digits[6],digits[1], sep = "")) %in% wantedPrimes &
      as.integer(paste(digits[3],digits[4],digits[5],digits[6],digits[1],digits[2], sep = "")) %in% wantedPrimes &
      as.integer(paste(digits[4],digits[5],digits[6],digits[1],digits[2],digits[3], sep = "")) %in% wantedPrimes &
      as.integer(paste(digits[5],digits[6],digits[1],digits[2],digits[3],digits[4], sep = "")) %in% wantedPrimes &
      as.integer(paste(digits[6],digits[1],digits[2],digits[3],digits[4],digits[5], sep = "")) %in% wantedPrimes
      ) {winners <- c(winners, wantedPrimes[i]); next}
}

length(winners) + 13   # 13 primes less than 100 were given in the question.

# Problem 36 - Double-base palindromes - SOLVED

dec2bin <- function(x){
  quotient <- x
  bits <- c()
  while(!(quotient == 0)){
    bits <- c(bits, quotient %% 2)
    quotient <- floor(quotient/2)
  }
  paste(rev(bits), collapse = "")
}

nums <- seq(1, 999999, 2)
decPalins <- c()
for(i in 1:length(nums)){
  if(as.character(nums[i]) == paste(rev(unlist(strsplit(as.character(nums[i]), split = ""))), collapse = "")) decPalins <- c(decPalins, nums[i])
}

binPalins <- c()
for(i in 1:length(decPalins)){
  if(dec2bin(decPalins[i]) == paste(rev(unlist(strsplit(dec2bin(decPalins[i]), split = ""))), collapse = "")) binPalins <- c(binPalins, decPalins[i])
}
sum(binPalins)

# Problem 37 - Truncatable primes - SOLVED

primesUnder <- function(x){
  nums <- 2:x
  for(i in 1:length(nums)){
    if(nums[i] > sqrt(x)) break
    nums <- nums[nums == nums[i] | !(nums %% nums[i] == 0)]
  }
  nums
}
primes <- primesUnder(1000000)
newPrimes <- c()
for(i in 1:length(primes)){
  if(!(TRUE %in% (c(4,6,8,0) %in% as.integer(unlist(strsplit(as.character(primes[i]), split = "")))))) newPrimes <- c(newPrimes, primes[i])
}
wantedPrimes <- c()
for(i in 1:length(newPrimes)){
  if(length(wantedPrimes) == 11) break
  test <- newPrimes[i + 4]
  forwards <- test
  truncLeft <- c()
  for (i in 1:floor(log10(forwards))){
    forwards <- floor(forwards/10)
    truncLeft <- c(truncLeft, forwards)
  }
  if(FALSE %in% (truncLeft %in% newPrimes)) next
  backwards <- as.integer(paste(rev(unlist(strsplit(as.character(test), split = ""))), collapse = ""))
  truncRight <- c()
  for (i in 1:floor(log10(backwards))){
    backwards <- floor(backwards/10)
    truncRight <- c(truncRight, as.integer(paste(rev(unlist(strsplit(as.character(backwards), split = ""))), collapse = "")))
  }
  if(!(FALSE %in% (truncRight %in% newPrimes))) wantedPrimes <- c(wantedPrimes, test)
}
sum(wantedPrimes)

# Problem 38 - Pandigital multiples - SOLVED

maxProduct <- 0

# 4-digit & 5-digit

for(i in 5000:9999){
  product <- paste(i * c(1:2), collapse = "")
  if(!(paste(sort(as.numeric(unlist(strsplit(product, split = "")))), collapse = "") == "123456789")) next
  if(as.numeric(product) > maxProduct) maxProduct <- as.numeric(product)
}

# three 3-digits

for(i in 100:333){
  product <- paste(i * c(1:3), collapse = "")
  if(!(paste(sort(as.numeric(unlist(strsplit(product, split = "")))), collapse = "") == "123456789")) next
  if(as.numeric(product) > maxProduct) maxProduct <- as.numeric(product)
}

# three 2-digits & 3-digit

for(i in 25:33){
  product <- paste(i * c(1:4), collapse = "")
  if(!(paste(sort(as.numeric(unlist(strsplit(product, split = "")))), collapse = "") == "123456789")) next
  if(as.numeric(product) > maxProduct) maxProduct <- as.numeric(product)
}

# 1-digit & 4 2-digits

for(i in 5:9){
  product <- paste(i * c(1:5), collapse = "")
  if(!(paste(sort(as.numeric(unlist(strsplit(product, split = "")))), collapse = "") == "123456789")) next
  if(as.numeric(product) > maxProduct) maxProduct <- as.numeric(product)
}

# 9 1-digits

for(i in 1){
  product <- paste(i * c(1:9), collapse = "")
  if(!(paste(sort(as.numeric(unlist(strsplit(product, split = "")))), collapse = "") == "123456789")) next
  if(as.numeric(product) > maxProduct) maxProduct <- as.numeric(product)
}

maxProduct

# Problem 39 - Integer right triangles - SOLVED

p <- c()
for(a in 1:500){
  for(b in 1:500){
    if(a + b > 500) break
    c <- sqrt(a^2 + b^2)
    if(!(round(c, 0) == c)) next
    p <- c(p, a+b+c)
  }
}

head(sort(table(p), decreasing = TRUE),1)

# Problem 40 - Champernowne's constant - SOLVED

digits <- as.numeric(unlist(strsplit(paste(c(1:200000), collapse = ""), split = "")))
digits[1]*digits[10]*digits[100]*digits[1000]*digits[10000]*digits[100000]*digits[1000000]

# Problem 41 - Pandigital prime - SOLVED

primesUnder <- function(x){
  nums <- 2:x
  for(i in 1:length(nums)){
    if(i > sqrt(x)) break
    nums <- nums[nums == nums[i] | !(nums %% nums[i] == 0)]
  }
  nums
}
primes <- sort(primesUnder(7654321), decreasing = TRUE)
for(i in 1:length(primes)){
  if(paste(sort(unlist(strsplit(as.character(primes[i]), split = ""))), collapse = "") == "1234567") {print(primes[i]); break}
}

# Problem 42 - Coded triangle numbers - SOLVED

wordsConnect <- file(description = "p042_words.txt", open = "r", blocking = TRUE)
words <- readLines(wordsConnect)
words <- unlist(strsplit(words, split = "\",\""))
words[1] <- "A"
words[length(words)] <- "YOUTH"

triangles <- c()
for(i in 1:50){
  triangles[i] <- 0.5*i*(i+1)
}

counter <- 0
for(i in 1:length(words)){
  test <- unlist(strsplit(words[i], split = ""))
  letterCounter <- 0
  for(j in 1:length(test)){
    letterCounter <- letterCounter + which(LETTERS == test[j])
  }
  if(letterCounter %in% triangles) counter <- counter + 1
}

counter

# Problem 43 - Sub-string divisibility - SOLVED

# d4 is in c(2,4,6,8,0)
# d6 is 5

# 952867,357289  <- d5,d6,d7,d8,d9,d10 possibilites

# 30952867   14
# 60357289   14
# 06357289   14

1430952867 + 4130952867 + 1460357289 + 4160357289 + 1406357289 + 4106357289

# Problem 44 - Pentagon numbers - SOLVED

length(pentagons)
pentagons <- c()
for(i in 1:2500){
  pentagons[i] <- i*(3*i-1)/2
}
additions <- c()
for(i in 1:length(pentagons)){
  for(j in 1:length(pentagons)){
    if((j >= i) | (pentagons[i] + pentagons[j]) > max(pentagons)) break
    if((pentagons[i] + pentagons[j]) %in% pentagons) additions <- rbind(additions, c(pentagons[i], pentagons[j]))
  }
}
differences <- c()
for(i in 1:length(additions[,1])){
  if((additions[i,][1] - additions[i,][2]) %in% pentagons) differences <- rbind(differences, additions[i,])
}
differences[1,1] - differences[1,2]

# Problem 45 - Triangular, pentagonal, and hexagonal - SOLVED

triangles <- c(); pentagons <- c(); hexagons <- c()
  
for(t in 287:90000){
  triangles <- c(triangles, t*(t+1)/2)
}
for(p in 166:90000){
  pentagons <- c(pentagons, p*(3*p-1)/2)
}
for(h in 144:90000){
  hexagons <- c(hexagons, h*(2*h-1))
}

for(i in 1:length(triangles)){
  if(triangles[i] %in% pentagons & triangles[i] %in% hexagons) {print(c(i,triangles[i])); break}
}

# Problem 46 - Goldbach's other conjecture - SOLVED

primesUnder <- function(x){
  nums <- 2:x
  for(i in 1:length(nums)){
    if(nums[i] > sqrt(x)) break
    nums <- nums[nums == nums[i] | !(nums %% nums[i] == 0)]
  }
  nums
}
primes <- primesUnder(1000000)
squareTerms <- 2*c((1:100)^2)
odds <- seq(3,999999,2)
composites <- odds[!(odds %in% primes)]
nums <- c()
for(i in 1:length(primes)){
  for(j in 1:length(squareTerms)){
    nums <- c(nums, primes[i] + squareTerms[j])
  }
}
head(sort(composites[!(composites %in% nums)]),1)

# Problem 47 - Distinct primes factors - SOLVED

primesUnder <- function(x){
  nums <- 2:x
  for(i in 1:length(nums)){
    if(nums[i] > sqrt(x)) break
    nums <- nums[nums == nums[i] | !(nums %% nums[i] == 0)]
  }
  nums
}
primes <- primesUnder(150000)
length(primes)
factors <- function(x){
  test <- x
  factorList <- rep(1,sqrt(length(primes)))
  for(i in 1:sqrt(length(primes))){
    if(test == 1) break
    if(test %in% primes) {factorList <- c(factorList, test); break}
    repeat{
      ifelse(test %% primes[i] == 0, {factorList[i] <- factorList[i]*primes[i]; test <- test/primes[i]}, break)
    }
  }
  factorList[!(factorList == 1)]
}
(prod(c(2,3,4,5,7,9,11,13,17,19,23,29,31,37,41,43)))^(1/4) # min possible number
possibles <- 26196:150000
checks <- list(c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,NA,NA,NA),c(NA,NA,NA,NA))
for(i in 1:length(possibles)){
  checks[[(possibles[i] %% 4) + 1]] <- factors(possibles[i])
  if(!(length(checks[[1]]) == 4)) next
  if(!(length(checks[[2]]) == 4)) next
  if(!(length(checks[[3]]) == 4)) next
  if(!(length(checks[[4]]) == 4)) next
  if(!(TRUE %in% duplicated(c(checks[[1]],checks[[2]],checks[[3]],checks[[4]])))) {print(possibles[i-3]); break}
}
checks

# Problem 48 - Self powers - SOLVED

nums <- 1:1000
nums <- nums[!(nums %% 10 == 0)]
summands <- c()
for(i in nums){
  powers <- as.numeric(i)
  for(j in 1:(i-1)){
    powers <- as.numeric(powers*i)
    if(log(powers, base = 10) >= 12) powers <- powers - floor(powers/10^floor(log(powers, base = 10)))*10^floor(log(powers, base = 10))
    if(log(powers, base = 10) >= 11) powers <- powers - floor(powers/10^floor(log(powers, base = 10)))*10^floor(log(powers, base = 10))
    if(log(powers, base = 10) >= 10) powers <- powers - floor(powers/10^floor(log(powers, base = 10)))*10^floor(log(powers, base = 10))
  }
  summands <- c(summands, powers)
}
grandSum <- 0
for(i in 1:length(summands)){
  grandSum <- grandSum + summands[i]
  if(log(grandSum, base = 10) >= 10) grandSum <- grandSum - floor(grandSum/10^floor(log(grandSum, base = 10)))*10^floor(log(grandSum, base = 10))
}
grandSum

# Problem 49 - Prime Permutations - SOLVED

primesUnder <- function(x){
  nums <- 2:x
  for(i in 1:length(nums)){
    if(i > sqrt(x)) break
    nums <- nums[nums == nums[i] | !(nums %% nums[i] == 0)]
  }
  nums
}

primes <- primesUnder(9999)[primesUnder(9999) >= 1000]
perms <- primes
for(i in 1:length(perms)){
  perms[i] <- as.numeric(paste(sort(unlist(strsplit(as.character(perms[i]), split = "")), decreasing = TRUE), collapse = ""))
}

permsList <- list()
for(i in 1:length(perms)){
  if(is.na(primes[i])) next
  permsList <- append(permsList, list(sort(primes[which(perms == perms[i])], decreasing = TRUE)))
  primes[which(perms == perms[i])] <- NA
}

subtractions <- c()
winners <- list()
for(i in 1:length(permsList)){
  for(j in 1:length(permsList[[i]])){
    subtractions <- c(permsList[[i]][j] - permsList[[i]][-j])
    subtractions <- subtractions[subtractions > 0]
    subtractions <- c(subtractions, 2*subtractions)
    if(TRUE %in% duplicated(subtractions)) winners <- append(winners, list(sort(c(permsList[[i]][j + which(subtractions == subtractions[which(duplicated(subtractions) == TRUE)]) %% length(subtractions)/2],permsList[[i]][j]))))
  }
}
winners
paste(winners[[2]], collapse = "")

# Problem 50 - Consecutive prime sum - SOLVED

primesUnder <- function(x){
  nums <- 2:x
  for(i in 1:length(nums)){
    if(nums[i] > sqrt(x)) break
    nums <- nums[nums == nums[i] | !(nums %% nums[i] == 0)]
  }
  nums
}

primes <- primesUnder(10^6)

# Find upper bound for length

counter <- 0
for(i in 1:length(primes)){
  counter <- counter + i
  if(counter > 10^6) {print(c(i,counter)); break}
}

winner <- c(0,0)
names(winner) <- c("Prime","Sum Length")
counter <- 0
for(i in 1:1414){
  counter <- sum(primes[i:(i+winner[2])])
  for(j in (i+winner[2]+1):1414){
    counter <- counter + primes[j]
    if(counter > 10^6) break
    if(!(counter %in% primes)) next
    if(j - i + 1 > winner[2]) {winner[1] <- counter; winner[2] <- j - i + 1}
  }
  if(winner[2]*primes[i] > 10^6) break
}

winner

# Problem 51 - Prime digit replacements - SOLVED

primesUnder <- function(x){
  nums <- 2:x
  for(i in 1:length(nums)){
    if(nums[i] > sqrt(x)) break
    nums <- nums[nums == nums[i] | !(nums %% nums[i] == 0)]
  }
  nums
}
primes <- primesUnder(10^6)
primesToTest <- primes[primes > 10^4]
primesToCompareWith <- primes[primes > 10^5]
wantedPrimes <- c()
for(i in 1:length(primesToTest)){
  test <- unlist(strsplit(as.character(primesToTest[i]), split = ""))
  if(TRUE %in% duplicated(test[duplicated(test)])) wantedPrimes <- c(wantedPrimes, primesToTest[i]) # stores all primes with >3 duplicates
}
for(i in 1:length(wantedPrimes)){
  test <- unlist(strsplit(as.character(wantedPrimes[i]), split = ""))
  if(!(length(which(duplicated(test[duplicated(test)]))) == 1)) next # stores all primes with >4 duplicates
  if(test[duplicated(test)][which(duplicated(test[duplicated(test)]))] == test[length(test)]) wantedPrimes[i] <- NA # if final digit is a duplicate, discards that prime
}
wantedPrimes <- wantedPrimes[!is.na(wantedPrimes)]

for(m in 1:length(wantedPrimes)){
  
  if(is.na(wantedPrimes[m])) next
  
  test <- wantedPrimes[[m]]
  test <- unlist(strsplit(as.character(test), split = ""))
  duplicateTest <- test[duplicated(test)]
  indices <- which(test == unique(duplicateTest[duplicated(duplicateTest)]))
  
  # For the 3-duplicates
  
  if(length(indices) == 3) { 
    checks <- list()
    counter <- 0
    for(i in 0:9){
      test[indices] <- i; checks <- append(checks, list(test)) # performs all replacements of the 3 duplicates
    }
    
    checkswith3 <- c()
    for(i in 1:10){ # separates the 3-duplicates from the 4-duplicates (for discarding purposes)
      duplicateTest <- checks[[i]][duplicated(checks[[i]])]
      duplicateTest <- duplicateTest[duplicated(duplicateTest)]
      if(length(duplicateTest) == 1) checkswith3 <- c(checkswith3, as.numeric(paste(checks[[i]], collapse = "")))
    }
    
    for(i in 1:10){ # checks for an 8-prime family
      if(!(as.numeric(paste(checks[[i]], collapse = "")) %in% primesToCompareWith)) counter <- counter + 1
      if(counter == 3) {wantedPrimes[which(wantedPrimes %in% checkswith3)] <- NA; break}
    }
    
  }
  
  # For the 4-duplicates
  
  if(length(indices) == 4) { # need to perform 4 different checks for this
    for(j in 1:4){
    test <- wantedPrimes[[m]]
    test <- unlist(strsplit(as.character(test), split = ""))
    checks <- list()
    counter <- 0
    indicesfor4s <- indices[-j]
    for(i in 0:9){
      test[indicesfor4s] <- i; checks <- append(checks, list(test)) # performs all replacements of the 3 duplicates
    }
    checkswith3 <- c()
    for(i in 1:10){ # separates the 3-duplicates from the 4-duplicates (for discarding purposes)
      duplicateTest <- checks[[i]][duplicated(checks[[i]])]
      duplicateTest <- duplicateTest[duplicated(duplicateTest)]
      if(length(duplicateTest) == 1) checkswith3 <- c(checkswith3, as.numeric(paste(checks[[i]], collapse = "")))
    }
    
    for(i in 1:10){ # checks for an 8-prime family
      if(!(as.numeric(paste(checks[[i]], collapse = "")) %in% primesToCompareWith)) counter <- counter + 1
      if(counter == 3) {wantedPrimes[which(wantedPrimes %in% checkswith3)] <- NA; break}
    }
  }
  }
  
  # For the 5-duplicates
  
  if(length(indices) == 5) { # need to perform 5C3 different checks for this
    for(j in 1:5){
      for(k in 1:5){
      if(j >= k) next
      test <- wantedPrimes[[m]]
      test <- unlist(strsplit(as.character(test), split = ""))
      checks <- list()
      counter <- 0
      indicesfor5s <- indices[-c(j,k)]
      for(i in 0:9){
        test[indicesfor5s] <- i; checks <- append(checks, list(test)) # performs all replacements of the 3 duplicates
      }
      checkswith3 <- c()
      for(i in 1:10){ # separates the 3-duplicates from the 4-duplicates (for discarding purposes)
        duplicateTest <- checks[[i]][duplicated(checks[[i]])]
        duplicateTest <- duplicateTest[duplicated(duplicateTest)]
        if(length(duplicateTest) == 1) checkswith3 <- c(checkswith3, as.numeric(paste(checks[[i]], collapse = "")))
      }
      
      for(i in 1:10){ # checks for an 8-prime family
        if(!(as.numeric(paste(checks[[i]], collapse = "")) %in% primesToCompareWith)) counter <- counter + 1
        if(counter == 3) {wantedPrimes[which(wantedPrimes %in% checkswith3)] <- NA; break}
      }
    }
  }
  }
if(counter < 3) break
}

family8 <- c()
for(i in 1:10){
  family8 <- c(family8, as.numeric(paste(checks[[i]], collapse = "")))
}
min(family8[family8 %in% primesToCompareWith])

# Problem 52 - Permuted multiples - SOLVED

nums <- 1:(10^7)
wantedNums <- c()
for(j in 1:7){
    wantedNums <- c(wantedNums, nums[nums >= 10^(j-1) & nums < (10^j)/6])
    nums <- nums[nums >= 10^j]
}
length(wantedNums)
for(i in 1:length(wantedNums)){
  num <- wantedNums[i]
  test <- as.numeric(paste(sort(unlist(strsplit(as.character(num), split = ""))), collapse = ""))
  if(!(as.numeric(paste(sort(unlist(strsplit(as.character(2*num), split = ""))), collapse = "")) == test)) next
  if(!(as.numeric(paste(sort(unlist(strsplit(as.character(3*num), split = ""))), collapse = "")) == test)) next
  if(!(as.numeric(paste(sort(unlist(strsplit(as.character(4*num), split = ""))), collapse = "")) == test)) next
  if(!(as.numeric(paste(sort(unlist(strsplit(as.character(5*num), split = ""))), collapse = "")) == test)) next
  if(as.numeric(paste(sort(unlist(strsplit(as.character(6*num), split = ""))), collapse = "")) == test) {print(num); break}
}

# Problem 53 - Combinatoric selections - SOLVED

counter <- 0
for(n in 23:100){
  for(r in 1:n){
    if(!(choose(n,r) > 10^6)) next
    minR <- r
    maxR <- n - r
    counter <- counter + (maxR - minR + 1)
    break
  }
}
counter

# Problem 54 - Poker hand - SOLVED

# Reading in the games to test

handsConnect <- file(description = "p054_poker.txt", open = "r", blocking = TRUE)
hands <- readLines(handsConnect)

# Possible card values

values <- c(2, 3, 4, 5, 6, 7, 8, 9, "T", "J", "Q", "K", "A")
values <- values[length(values):1]

# Test if a hand contains consecutive values

consecTest <- function(x){
  vals <- c()
  bool <- FALSE
  consecTestVec <- which(values %in% x)
  for(i in 2:5){
    vals[i-1] <- consecTestVec[i] - consecTestVec[i-1]
  }
  if(!(FALSE %in% (vals %in% 1))) bool <- TRUE
  bool
}

# Find the values of all cards (for tiebreaker purposes)

handValues <- function(x){
  for(i in 1:length(x)){
    x[i] <- which(values == x[i])
  }
  x <- as.numeric(x)
  x <- sort(table(x), decreasing = TRUE)
  newSort <- c()
  for(i in 1:length(x)){
    newSort <- c(newSort, rep(names(x)[i], x[i])) 
  }
  as.numeric(newSort)
}

p1wins <- 0
p2wins <- 0

for(j in 1:1000){
  hand <- hands[j]
  hand <- unlist(strsplit(hand, split = " "))
  
  # Creating data frames of each player's hands
  
  p1 <- hand[1:5]
  p1 <- unlist(strsplit(p1, split = ""))
  valuesp1 <- p1[seq(1,10,2)]
  suitsp1 <- p1[seq(2,10,2)]
  p1 <- as.data.frame(cbind(valuesp1,suitsp1))
  p2 <- hand[6:10]
  p2 <- unlist(strsplit(p2, split = ""))
  valuesp2 <- p2[seq(1,10,2)]
  suitsp2 <- p2[seq(2,10,2)]
  p2 <- as.data.frame(cbind(valuesp2,suitsp2))
  
  # Finding Player 1's hand's rank
  
  result1 <- character(0)
  handValue1 <- 0
  
  p1counts <- sort(table(p1$valuesp1), decreasing = TRUE)
  if(!(FALSE %in% (values[1:5] %in% p1$valuesp1)) && length(unique(p1$suitsp1)) == 1) {
    result1 <- 1 # "Royal Flush"
  } else if(length(unique(p1$suitsp1)) == 1 && consecTest(p1$valuesp1)) {
    result1 <- 2 # "Straight Flush"
  } else if(4 %in% p1counts) {
    result1 <- 3 # "Four of a Kind"
  } else if(!(FALSE %in% (p1counts == c(3,2)))) {
    result1 <- 4 # "Full House"
  } else if(length(unique(p1$suitsp1)) == 1) {
    result1 <- 5 # "Flush"
  } else if(consecTest(p1$valuesp1)) {
    result1 <- 6 # "Straight"
  } else if(3 %in% p1counts) {
    result1 <- 7 # "Three of a Kind"
  } else if(length(names(p1counts[p1counts == 2])) == 2) {
    result1 <- 8 # "Two Pairs"
  } else if(!(FALSE %in% (p1counts == c(2,1,1,1)))) {
    result1 <- 9 # "One Pair"
  } else {
    result1 <- 10 # "High Card"
  }
  
  # Finding Player 2's hand's rank
  
  result2 <- character(0)
  handValue2 <- 0
  
  p2counts <- sort(table(p2$valuesp2), decreasing = TRUE)
  if(!(FALSE %in% (values[1:5] %in% p2$valuesp2)) && length(unique(p2$suitsp2)) == 1) {
    result2 <- 1 # "Royal Flush"
  } else if(length(unique(p2$suitsp2)) == 1 && consecTest(p2$valuesp2)) {
    result2 <- 2 # "Straight Flush"
  } else if(4 %in% p2counts) {
    result2 <- 3 # "Four of a Kind"
  } else if(!(FALSE %in% (p2counts == c(3,2)))) {
    result2 <- 4 # "Full House"
  } else if(length(unique(p2$suitsp2)) == 1) {
    result2 <- 5 # "Flush"
  } else if(consecTest(p2$valuesp2)) {
    result2 <- 6 # "Straight"
  } else if(3 %in% p2counts) {
    result2 <- 7 # "Three of a Kind"
  } else if(length(names(p2counts[p2counts == 2])) == 2) {
    result2 <- 8 # "Two Pairs"
  } else if(!(FALSE %in% (p2counts == c(2,1,1,1)))) {
    result2 <- 9 # "One Pair"
  } else {
    result2 <- 10 # "High Card"
  }
  
  if(result1 < result2){
    p1wins <- p1wins + 1
  } else if(result1 > result2){
    p2wins <- p2wins + 1
  } else {
    handValue1 <- handValues(p1$valuesp1); handValue2 <- handValues(p2$valuesp2)
  }
  
  if(result1 == result2){
    for(i in 1:length(handValue1)){
      if(handValue1[i] == handValue2[i]) next
      if(handValue1[i] < handValue2[i]){
        p1wins <- p1wins + 1; break
      } else {
        p2wins <- p2wins + 1; break
      }
    }
  }
}
p1wins

# Problem 55 - Lychrel numbers - SOLVED

options(scipen = 999)

testIfPalin <- function(x){
  bool <- FALSE
  test <- unlist(strsplit(as.character(x), split = ""))
  test <- as.numeric(paste(test[length(test):1], collapse = ""))
  if(x == test) bool <- TRUE
  bool
}

nums <- as.numeric(1:10000)

nonLychrel <- 0

for(i in nums){
  test <- i
  for(j in 1:50){
    palin <- unlist(strsplit(as.character(test), split = ""))
    palin <- as.numeric(paste(palin[length(palin):1], collapse = ""))
    if(testIfPalin(test+palin)) {nonLychrel <- nonLychrel + 1; break}
    test <- test+palin
  }
}

10000 - nonLychrel

# Problem 56 - Powerful digit sum - SOLVED

install.packages("gmp")
library(gmp)

allSums <- c(0)
for(a in 99:1){
  test <- a
  if(9*log10(as.bigz(a)^99) < head(sort(allSums, decreasing = TRUE), 1)) break
  for(b in a:1){
    allSums <- c(allSums, sum(as.numeric(unlist(strsplit(as.character(as.bigz(a)^b), split = "")))))
    if(9*log10(as.bigz(a)^b) < head(sort(allSums, decreasing = TRUE), 1)) break
  }
}
head(sort(allSums, decreasing = TRUE), 1)

# Problem 57 - Square root convergents - SOLVED

additions <- as.bigz(c(4,10))
for(i in 1:998){
  additions <- c(additions, additions[i] + 2*additions[i+1])
}
numerators <- as.bigz(c(3))
for(i in 1:999){
  numerators <- c(numerators, numerators[i] + additions[i])
}
denominators <- as.bigz(c(2))
for(i in 1:999){
  denominators <- c(denominators, denominators[i] + numerators[i])
}

counter <- 0
for(i in 1:1000){
  if(ceiling(log10(numerators[i])) > ceiling(log10(denominators[i]))) counter <- counter + 1
}
counter

# Problem 58 - Spiral primes - SOLVED

primesUnder <- function(x){
  nums <- 2:x
  for(i in 1:length(nums)){
    if(i > sqrt(x)) break
    nums <- nums[nums == nums[i] | !((nums %% nums[i]) == 0)]
  }
  nums
}
topR <- c(1)
for(i in 1:20000){
  topR <- c(topR, 2 + topR[i] + 8*(i-1))
}
topL <- c(1)
for(i in 1:20000){
  topL <- c(topL, 4 + topL[i] + 8*(i-1))
}
bottomL <- c(1)
for(i in 1:20000){
  bottomL <- c(bottomL, 6 + bottomL[i] + 8*(i-1))
}

primes <- primesUnder(sqrt(max(bottomL)))

primeOrNot <- c(0)
for(i in 2:20000){
  primeOrNot <- c(primeOrNot, !(0 %in% (topR[i] %% primes[primes < sqrt(topR[i])])), !(0 %in% (topL[i] %% primes[primes < sqrt(topL[i])])), !(0 %in% (bottomL[i] %% primes[primes < sqrt(bottomL[i])])), 0)
  if(mean(primeOrNot) < 0.1) {print(2*i - 1); break}
}

# Problem 59 - XOR decryption - SOLVED

setwd("C:/Users/jackj.LAPTOP-U1V11TR1/Documents/R Projects")

dec2bin <- function(x){
  bin <- c()
  while(x > 0){
    if(x %% 2 == 0) {
      bin <- c(bin, "0")
    } else bin <- c(bin, "1")
    x <- floor(x/2)
  }
  paste(rev(bin), collapse = "")
}

xorFunc <- function(x,y){
  num1 <- max(c(x,y)); num2 <- min(c(x,y))
  num1 <- as.numeric(rev(unlist(strsplit(dec2bin(num1), split = ""))))
  num2 <- as.numeric(rev(unlist(strsplit(dec2bin(num2), split = ""))))
  binSum <- c()
  for(i in 1:length(num1)){
    if(is.na(num2[i])) {binSum[i] <- num1[i]; next}
    binSum <- c(binSum, (num1[i] + num2[i]) %% 2)
  }
  decSum <- 0
  for(i in 1:length(binSum)){
    decSum <- decSum + (2^(i-1))*binSum[i]
  }
  decSum
}

msgConnect <- file(description = "p059_cipher.txt", open = "r", blocking = TRUE)
msg <- readLines(msgConnect)
msg <- as.numeric(unlist(strsplit(msg, split = ",")))

# Finding the space bar character for each key

sort(round(100*table(msg[seq(1,length(msg),3)])/length(seq(1,length(msg),3)),2), decreasing = TRUE) # First Key
sort(round(100*table(msg[seq(2,length(msg),3)])/length(seq(2,length(msg),3)),2), decreasing = TRUE) # Second Key
sort(round(100*table(msg[seq(3,length(msg),3)])/length(seq(3,length(msg),3)),2), decreasing = TRUE) # Third Key

# 69, 88, 80; if these are the space bar, they would have to turn to 32

possibleKey <- c(xorFunc(69,32), xorFunc(88,32), xorFunc(80,32)) # 101,120,112
ascii <- c(97:122, 65:90, 32, 34, 39:59, 91, 93)
names(ascii) <- c(letters, LETTERS, " ", '"', "'", "(", ")", "*", "+", ",", "-", ".", "/", 0:9, ":", ";", "[", "]")
decrypt <- msg
for(i in 0:(length(decrypt)-1)){
  decrypt[i + 1] <- xorFunc(decrypt[i + 1], possibleKey[(i %% 3) + 1])
}
words <- decrypt
for(i in 1:length(words)){
  if(length(ascii[ascii == words[i]]) == 0) next
  words[i] <- names(ascii[ascii == words[i]])
}
words <- paste(words, collapse = "")
sum(decrypt)
words

# Problem 60 - Prime pair sets - SOLVED

# Primes must be subset into either 1 mod 3 or 2 mod 3 groups (apart from 3)

primesUnder <- function(x){
  nums <- 2:x
  for(i in 1:length(nums)){
    if(i > sqrt(x)) break
    nums <- nums[nums == nums[i] | !((nums %% nums[i]) == 0)]
  }
  nums
}
primes <- primesUnder(13000)
primesToTest <- primes[!(primes == 2 | primes == 5)]
primesToTest1mod3 <- primesToTest[(primesToTest %% 3) == 1 | primesToTest == 3] # 3 included
primesToTest2mod3 <- primesToTest[(primesToTest %% 3) == 2 | primesToTest == 3] # 3 included
isPrime <- function(x){
  !(0 %in% (x %% primes[primes <= sqrt(x)]))
}

# 1 mod 3 primes: pairs

pairsdf1mod3 <- na.omit(as.data.frame(cbind(NA,NA)))
for(i in 1:length(primesToTest1mod3)){
  for(j in 1:length(primesToTest1mod3)){
    if(i == j | primesToTest1mod3[j] > 5210) break # Break point chosen from an earlier run's answer of 26,033
    if(isPrime(as.numeric(paste(c(primesToTest1mod3[i],primesToTest1mod3[j]), collapse = ""))) && isPrime(as.numeric(paste(c(primesToTest1mod3[j],primesToTest1mod3[i]), collapse = "")))) pairsdf1mod3 <- rbind(pairsdf1mod3, c(primesToTest1mod3[j], primesToTest1mod3[i]))
  }
}

# 2 mod 3 primes: pairs

pairsdf2mod3 <- na.omit(as.data.frame(cbind(NA,NA)))
for(i in 1:length(primesToTest2mod3)){
  for(j in 1:length(primesToTest2mod3)){
    if(i == j | primesToTest2mod3[j] > 5210) break # Break point chosen from an earlier run's answer of 26,033
    if(isPrime(as.numeric(paste(c(primesToTest2mod3[i],primesToTest2mod3[j]), collapse = ""))) && isPrime(as.numeric(paste(c(primesToTest2mod3[j],primesToTest2mod3[i]), collapse = "")))) pairsdf2mod3 <- rbind(pairsdf2mod3, c(primesToTest2mod3[j], primesToTest2mod3[i]))
  }
}
pairsdf2mod3 <- na.omit(pairsdf2mod3)

# 1 mod 3 primes: triples

triplesdf1mod3 <- na.omit(as.data.frame(cbind(NA,NA,NA)))
for(k in 1:length(unique(pairsdf1mod3[,1]))){
  firstPrime <- unique(pairsdf1mod3[,1])[k]
  testdf <- na.omit(pairsdf1mod3[pairsdf1mod3[,1] == firstPrime,])
  if(length(testdf[,1]) <= 1) next
  for(i in 1:length(testdf[,2])){
    for(j in 1:length(testdf[,2])){
      if(i == j) break
      if(isPrime(as.numeric(paste(c(testdf[,2][i],testdf[,2][j]), collapse = ""))) 
         && isPrime(as.numeric(paste(c(testdf[,2][j],testdf[,2][i]), collapse = "")))) {
        triplesdf1mod3 <- rbind(triplesdf1mod3, c(firstPrime, testdf[,2][j], testdf[,2][i]))
      }
    }
  } 
}
triplesdf1mod3 <- na.omit(triplesdf1mod3)

# 2 mod 3 primes: triples

triplesdf2mod3 <- na.omit(as.data.frame(cbind(NA,NA,NA)))
for(k in 1:length(unique(pairsdf2mod3[,1]))){
  firstPrime <- unique(pairsdf2mod3[,1])[k]
  testdf <- na.omit(pairsdf2mod3[pairsdf2mod3[,1] == firstPrime,])
  if(length(testdf[,1]) <= 1) next
  for(i in 1:length(testdf[,2])){
    for(j in 1:length(testdf[,2])){
      if(i == j) break
      if(isPrime(as.numeric(paste(c(testdf[,2][i],testdf[,2][j]), collapse = ""))) 
         && isPrime(as.numeric(paste(c(testdf[,2][j],testdf[,2][i]), collapse = "")))) {
        triplesdf2mod3 <- rbind(triplesdf2mod3, c(firstPrime, testdf[,2][j], testdf[,2][i]))
      }
    }
  } 
}
triplesdf2mod3 <- na.omit(triplesdf2mod3)

# 1 mod 3 primes: quads

quadsdf1mod3 <- na.omit(as.data.frame(cbind(NA,NA,NA,NA)))
for(l in 1:length(unique(triplesdf1mod3[,1]))){
  firstPrime <- unique(triplesdf1mod3[,1])[l]
  testdf <- na.omit(triplesdf1mod3[triplesdf1mod3 == firstPrime,])
  if(length(testdf[,1]) <= 1) next
  for(k in 1:length(unique(testdf[,2]))){
    secondPrime <- unique(testdf[,2])[k]
    testdf2 <- testdf[testdf[,2] == secondPrime,]
    if(length(testdf2[,2]) <= 1) next
    for(i in 1:length(testdf2[,3])){
      for(j in 1:length(testdf2[,3])){
        if(i == j) break
        if(isPrime(as.numeric(paste(c(testdf2[,3][i],testdf2[,3][j]), collapse = ""))) 
           && isPrime(as.numeric(paste(c(testdf2[,3][j],testdf2[,3][i]), collapse = "")))) {
          quadsdf1mod3 <- rbind(quadsdf1mod3, c(firstPrime, secondPrime, testdf2[,3][j], testdf2[,3][i]))
        }
      }
    }
  }
}
quadsdf1mod3 <- na.omit(quadsdf1mod3)

# 2 mod 3 primes: quads

quadsdf2mod3 <- na.omit(as.data.frame(cbind(NA,NA,NA,NA)))
for(l in 1:length(unique(triplesdf2mod3[,1]))){
  firstPrime <- unique(triplesdf2mod3[,1])[l]
  testdf <- na.omit(triplesdf2mod3[triplesdf2mod3 == firstPrime,])
  if(length(testdf[,1]) <= 1) next
  for(k in 1:length(unique(testdf[,2]))){
    secondPrime <- unique(testdf[,2])[k]
    testdf2 <- testdf[testdf[,2] == secondPrime,]
    if(length(testdf2[,2]) <= 1) next
    for(i in 1:length(testdf2[,3])){
      for(j in 1:length(testdf2[,3])){
        if(i == j) break
        if(isPrime(as.numeric(paste(c(testdf2[,3][i],testdf2[,3][j]), collapse = ""))) 
           && isPrime(as.numeric(paste(c(testdf2[,3][j],testdf2[,3][i]), collapse = "")))) {
          quadsdf2mod3 <- rbind(quadsdf2mod3, c(firstPrime, secondPrime, testdf2[,3][j], testdf2[,3][i]))
        }
      }
    }
  }
}
quadsdf2mod3 <- na.omit(quadsdf2mod3)

# 1 mod 3 primes: pents

pentsdf1mod3 <- na.omit(as.data.frame(cbind(NA,NA,NA,NA,NA)))
for(m in 1:length(unique(quadsdf1mod3[,1]))){
  firstPrime <- unique(quadsdf1mod3[,1])[m]
  testdf <- na.omit(quadsdf1mod3[quadsdf1mod3[,1] == firstPrime,])
  if(length(testdf[,1]) <= 1) next
  for(l in 1:length(unique(testdf[,2]))){
    secondPrime <- unique(testdf[,2])[l]
    testdf2 <- na.omit(testdf[testdf[,2] == secondPrime,])
    if(length(testdf2[,2]) <= 1) next
    for(k in 1:length(unique(testdf2[,3]))){
      thirdPrime <- unique(testdf2[,3])[k]
      testdf3 <- na.omit(testdf2[testdf2[,3] == thirdPrime,])
      if(length(testdf3[,3]) <= 1) next
      for(i in 1:length(testdf3[,4])){
        for(j in 1:length(testdf3[,4])){
          if(i == j) break
          if(isPrime(as.numeric(paste(c(testdf3[,4][i],testdf3[,4][j]), collapse = ""))) 
             && isPrime(as.numeric(paste(c(testdf3[,4][j],testdf3[,4][i]), collapse = "")))) {
            pentsdf1mod3 <- rbind(pentsdf1mod3, c(firstPrime, secondPrime, thirdPrime, testdf3[,4][j], testdf3[,4][i]))
          }
        }
      }
    }
  }
}
pentsdf1mod3 <- na.omit(pentsdf1mod3)
pentsdf1mod3

# 2 mod 3 primes: pents

pentsdf2mod3 <- na.omit(as.data.frame(cbind(NA,NA,NA,NA,NA)))
for(m in 1:length(unique(quadsdf2mod3[,1]))){
  firstPrime <- unique(quadsdf2mod3[,1])[m]
  testdf <- na.omit(quadsdf2mod3[quadsdf2mod3[,1] == firstPrime,])
  if(length(testdf[,1]) <= 1) next
  for(l in 1:length(unique(testdf[,2]))){
    secondPrime <- unique(testdf[,2])[l]
    testdf2 <- na.omit(testdf[testdf[,2] == secondPrime,])
    if(length(testdf2[,2]) <= 1) next
    for(k in 1:length(unique(testdf2[,3]))){
      thirdPrime <- unique(testdf2[,3])[k]
      testdf3 <- na.omit(testdf2[testdf2[,3] == thirdPrime,])
      if(length(testdf3[,3]) <= 1) next
      for(i in 1:length(testdf3[,4])){
        for(j in 1:length(testdf3[,4])){
          if(i == j) break
          if(isPrime(as.numeric(paste(c(testdf3[,4][i],testdf3[,4][j]), collapse = ""))) 
             && isPrime(as.numeric(paste(c(testdf3[,4][j],testdf3[,4][i]), collapse = "")))) {
            pentsdf2mod3 <- rbind(pentsdf2mod3, c(firstPrime, secondPrime, thirdPrime, testdf3[,4][j], testdf3[,4][i]))
          }
        }
      }
    }
  }
}
pentsdf2mod3 <- na.omit(pentsdf2mod3)

minSum <- 999999
if(length(pentsdf1mod3[,1])){
  for(i in 1:length(pentsdf1mod3[,1])){
    minSum <- min(minSum, sum(pentsdf1mod3[i,]))
  }
}
if(length(pentsdf2mod3[,1]) > 0){
  for(i in 1:length(pentsdf2mod3[,1])){
    minSum <- min(minSum, sum(pentsdf1mod3[i,]))
  }
}

minSum


primes <- primesUnder(minSum)
finalTestPrimes <- primes[primes > 13000]
for(i in 1:length(quadsdf1mod3[,1])){
  testGroup <- quadsdf1mod3[i,]
  testPrimes <- finalTestPrimes[finalTestPrimes < minSum - sum(testGroup)]
  if(length(testPrimes) == 0) next
  for(j in 1:length(testPrimes)){
    finalTestPrime <- testPrimes[j]
    for(k in 1:4){
      if(!(isPrime(as.numeric(paste(c(testGroup[k],finalTestPrime), collapse = "")))
         && isPrime(as.numeric(paste(c(finalTestPrime, testGroup[k]), collapse = ""))))) break
      if(k == 4) pentsdf1mod3 <- rbind(pentsdf1mod3, c(testGroup, finalTestPrime))
    }
  }
}
for(i in 1:length(quadsdf2mod3[,1])){
  testGroup <- quadsdf2mod3[i,]
  testPrimes <- finalTestPrimes[finalTestPrimes < minSum - sum(testGroup)]
  if(length(testPrimes) == 0) next
  for(j in 1:length(testPrimes)){
    finalTestPrime <- testPrimes[j]
    for(k in 1:4){
      if(!(isPrime(as.numeric(paste(c(testGroup[k],finalTestPrime), collapse = "")))
           && isPrime(as.numeric(paste(c(finalTestPrime, testGroup[k]), collapse = ""))))) break
      if(k == 4) pentsdf2mod3 <- rbind(pentsdf2mod3, c(testGroup, finalTestPrime))
    }
  }
}

if(length(pentsdf1mod3[,1]) > 0){
  for(i in 1:length(pentsdf1mod3[,1])){
    minSum <- min(minSum, sum(pentsdf1mod3[i,]))
  }
}
if(length(pentsdf2mod3[,1]) > 0){
  for(i in 1:length(pentsdf2mod3[,1])){
    minSum <- min(minSum, sum(pentsdf1mod3[i,]))
  }
}
minSum

# Problem 61 - Cyclical figurate numbers - SOLVED

triangles <- c()
for(i in 1:10000){
  triangles[i] <- i*(i+1)/2
  if(triangles[i] > 10000) break
}
triangles <- triangles[triangles > 1000 & triangles < 10000]

squares <- c()
for(i in 1:10000){
  squares[i] <- i^2
  if(squares[i] > 10000) break
}
squares <- squares[squares > 1000 & squares < 10000]

pents <- c()
for(i in 1:10000){
  pents[i] <- i*(3*i-1)/2
  if(pents[i] > 10000) break
}
pents <- pents[pents > 1000 & pents < 10000]

hexes <- c()
for(i in 1:10000){
  hexes[i] <- i*(2*i-1)
  if(hexes[i] > 10000) break
}
hexes <- hexes[hexes > 1000 & hexes < 10000]

hepts <- c()
for(i in 1:10000){
  hepts[i] <- i*(5*i-3)/2
  if(hepts[i] > 10000) break
}
hepts <- hepts[hepts > 1000 & hepts < 10000]

octs <- c()
for(i in 1:10000){
  octs[i] <- i*(3*i-2)
  if(octs[i] > 10000) break
}
octs <- octs[octs > 1000 & octs < 10000]

triangles <- triangles[triangles - 100*floor(triangles/100) > 9]
squares <- squares[squares - 100*floor(squares/100) > 9]
pents <- pents[pents - 100*floor(pents/100) > 9]
hexes <- hexes[hexes - 100*floor(hexes/100) > 9]
hepts <- hepts[hepts - 100*floor(hepts/100) > 9]
octs <- octs[octs - 100*floor(octs/100) > 9]

triangleStarts <- floor(triangles/100)
squareStarts <- floor(squares/100)
pentStarts <- floor(pents/100)
hexStarts <- floor(hexes/100)
heptStarts <- floor(hepts/100)
octStarts <- floor(octs/100)

triangleEnds <- triangles - 100*floor(triangles/100)
squareEnds <- squares - 100*floor(squares/100)
pentEnds <- pents - 100*floor(pents/100)
hexEnds <- hexes - 100*floor(hexes/100)
heptEnds <- hepts - 100*floor(hepts/100)
octEnds <- octs - 100*floor(octs/100)

numsList <- list(triangles, squares, pents, hexes, hepts)
startsList <- list(triangleStarts, squareStarts, pentStarts, hexStarts, heptStarts)
endsList <- list(triangleEnds, squareEnds, pentEnds, hexEnds, heptEnds)

perms <- na.omit(as.data.frame(c(NA,NA,NA,NA,NA)))
for(i in 1:5){
  for(j in 1:5){
    if(i == j) next
    for(k in 1:5){
      if(i == k | j == k) next
      for(l in 1:5){
        if(i == l | j == l | k == l) next
        for(m in 1:5){
          if(i == m | j == m | k == m | l == m) next
          perms <- rbind(perms, c(i,j,k,l,m))
        }
      }
    }
  }
}

answer <- c()
for(i in 1:length(octs)){
  if(length(answer) > 0) break
  test <- octEnds[i]
  for(j in 1:nrow(perms)){
    if(!(test %in% startsList[[perms[j,1]]])) next
    test2 <- endsList[[perms[j,1]]][which(startsList[[perms[j,1]]] == test)]
    for(k in 1:length(test2)){
      if(!(test2[k] %in% startsList[[perms[j,2]]])) next
      test3 <- endsList[[perms[j,2]]][which(startsList[[perms[j,2]]] == test2[k])]
      for(l in 1:length(test3)){
        if(!(test3[l] %in% startsList[[perms[j,3]]])) next
        test4 <- endsList[[perms[j,3]]][which(startsList[[perms[j,3]]] == test3[l])]
        for(m in 1:length(test4)){
          if(!(test4[m] %in% startsList[[perms[j,4]]])) next
          test5 <- endsList[[perms[j,4]]][which(startsList[[perms[j,4]]] == test4[m])]
          for(n in 1:length(test5)){
            if(!(test5[n] %in% startsList[[perms[j,5]]])) next
            test6 <- endsList[[perms[j,5]]][which(startsList[[perms[j,5]]] == test5[n])]
            for(o in 1:length(test6)){
              if(!(test6[o] == octStarts[i])) next
              {answer <- c(octs[i],
                           numsList[[perms[j,1]]][which(startsList[[perms[j,1]]] == test)],
                           numsList[[perms[j,2]]][which(startsList[[perms[j,2]]] == test2[k])],
                           numsList[[perms[j,3]]][which(startsList[[perms[j,3]]] == test3[l])],
                           numsList[[perms[j,4]]][which(startsList[[perms[j,4]]] == test4[m])],
                           numsList[[perms[j,5]]][which(startsList[[perms[j,5]]] == test5[n])]); break}
            }
          }
        }
      }
    }
  }
}
sum(answer)
# 1281 %in% octs; 8128 %in% hexes; 2882 %in% pents; 8256 %in% triangles; 5625 %in% squares; 2512 %in% hepts

# Problem 62 - Cubic permutations - SOLVED

options(scipen = 999)

answer <- 0
power <- 8
while(answer == 0){
  base <- ceiling((10^((power-1)/3)))
  top <- floor((10^(power/3)))
  possibles <- c()
  for(i in base:top){
    possibles <- c(possibles, as.numeric(paste(sort(unlist(strsplit(as.character(i^3), split = "")), decreasing = TRUE), collapse = "")))
  }
  possibleMins <- c()
  if(5 %in% table(possibles)) {
    for(i in 1:length(table(possibles)[table(possibles) == 5])){
      possibleMins <- c(possibleMins, 
                        base + min(which(possibles == as.numeric(names(sort(table(possibles), decreasing = TRUE))[i]))) - 1)
    }
    {answer <- (min(possibleMins))^3; print(answer)}
  }
  power <- power + 1
}

# Problem 63 - Powerful digit counts - SOLVED

options(scipen = 999)

counter <- 0
for(i in 1:9){
  for(j in 1:100){
    if(ceiling(log10(i^j)) == j | i^j == 10^(j-1)) counter <- counter + 1
    if(i^j < 10^(j - 1)) break
  }
}
counter

# Problem 64 - Odd period square roots - SOLVED

squares <- (1:100)^2
irrationals <- 1:10000
irrationals <- irrationals[!(irrationals %in% squares)]

newFrac <- function(N,a,d){
  N <- sqrt(N)
  d <- (N+a)*(N-a)/d
  for(i in 1:1000000){
    a <- a - d
    # whole <- whole + 1
    if(a < 0 && Mod(a) > N) {a <- Mod(a) - d; break}
  }
  round(c(a,d))
}

periods <- c()
for(i in 1:length(irrationals)){
  N <- irrationals[i]
  a <- floor(sqrt(N))
  d <- 1
  nextStep <- newFrac(N,a,d)
  for(j in 1:1000000){
    newa <- nextStep[1]; newd <- nextStep[2]
    if(j == 1) test <- c(newa, newd)
    nextStep <- newFrac(N,newa,newd)
    if(j > 1 && newa == test[1] && newd == test[2]) {periods[i] <- j - 1; break}
  }
}

length(periods[(periods %% 2) == 1])

# Problem 65 - Convergents of e - SOLVED

options(scipen = 999)
library(gmp)

nums <- c(2,rep(1,99))
for(i in 1:99){
  if((i %% 3) == 0) {nums[i] <- 2*i/3; next}
}

nextConvergent <- function(x,numer,denom){
  a <- x*denom+numer
  b <- denom
  c(b,a)
}

x <- nums[99]; numer <- 1; denom <- nums[100]
for(i in 98:1){
  nextStep <- nextConvergent(x,numer,denom)
  x <- as.bigz(nums[i]); numer <- as.bigz(nextStep[1]); denom <- as.bigz(nextStep[2])
  if(i == 1) winner <- nextConvergent(x,numer,denom)[2]
}

sum(as.numeric(unlist(strsplit(as.character(winner), split = ""))))

# Problem 66 - Diophantine equation

options(scipen = 999)
library(gmp)
install.packages("Rmpfr")
library(Rmpfr)

squares <- (1:100)^2
irrationals <- 1:10
irrationals <- irrationals[!(irrationals %in% squares)]

newFrac <- function(N,a,d){
  N <- sqrt(N)
  d <- (N+a)*(N-a)/d
  for(i in 1:1000000){
    a <- a - d
    # whole <- whole + 1
    if(a < 0 && Mod(a) > N) {a <- Mod(a) - d; break}
  }
  round(c(a,d))
}

periods <- c()
a
for(i in 1:length(irrationals)){
  N <- irrationals[i]
  a <- floor(sqrt(N))
  d <- 1
  nextStep <- newFrac(N,a,d)
  for(j in 1:1000000){
    newa <- nextStep[1]; newd <- nextStep[2]
    if(j == 1) test <- c(newa, newd)
    nextStep <- newFrac(N,newa,newd)
    if(j > 1 && newa == test[1] && newd == test[2]) {periods[i] <- j - 1; break}
  }
}

length(periods[(periods %% 2) == 1])



periods
