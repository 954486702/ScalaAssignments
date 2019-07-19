// Part 1 about the 3n+1 conjecture
//=================================


//(1) Complete the collatz function below. It should
//    recursively calculate the number of steps needed 
//    until the collatz series reaches the number 1.
//    If needed, you can use an auxiliary function that
//    performs the recursion. The function should expect
//    arguments in the range of 1 to 1 Million.
def collatz(n: Long): Long ={
    val count = collatzCount(n, 0)
    count
}

def collatzCount(n: Long, x:Long) : Long = {


    if(n==1) {
        x
    } 
    else if(n%2 == 0){
        //count(sum+1) 
        collatzCount(n/2, x+1)
    }  
    else {

        collatzCount(3*n + 1, x+1)
    }
}

//println(collatz(97))
//def count(sum: Long) : Long = {
//    sum
//}

//(2)  Complete the collatz_max function below. It should
//     calculate how many steps are needed for each number 
//     from 1 up to a bound and then calculate the maximum number of
//     steps and the corresponding number that needs that many 
//     steps. Again, you should expect bounds in the range of 1
//     up to 1 Million. The first component of the pair is
//     the maximum number of steps and the second is the 
//     corresponding number.

def collatz_max(bnd: Long) : (Long, Long) = {
    val collatzPairs = for(n <- List.range(1,bnd+1,1)) yield (collatz(n), n)
    val biggestPair = collatzPairs.maxBy(_._1)
    biggestPair
}
/*
println(collatz_max(100))
println(collatz_max(1000))
println(collatz_max(10000))
println(collatz_max(100000))
*/println(collatz_max(2))

