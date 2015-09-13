namespace Euler

module Math = 
    open System.Collections.Generic

    // Infinite fibo sequence with leading ones.
    let fibSeq = 
        Seq.unfold (fun (n, acc) -> 
            let curr = if acc = 0I then 1I else n+acc
            Some( curr, (acc, curr) )
        ) (0I, 0I)

    // Sieve of Sundaram up to n (long).
    let primeEratosthenes (n) = 
        seq {
            // Yield first prime.
            yield 2L;

            // Maintain a list of found composites.
            let composites = new HashSet<int64>()

            // Start from 3 and skip evens.
            for i in 3L .. 2L .. n do
                let found = composites.Contains(i)
                if not found then
                    yield i

                // Add composites to map (multiples of i upto n)
                for j in i .. i .. n do
                    composites.Add(j) |> ignore
        }

    // Largest prime
    let lpf n =
        let rec loop n = function
            |k when k*k >= n -> n
            |k when n % k = 0I -> loop (n/k) k
            |k -> loop n (k+1I)
        loop n 2I

module Problems =
    type 'a ll =
    | I of 'a             // leaf Item
    | L of 'a ll list     // ' <- confine the syntax colouring confusion

    // Result printer
    let printResult (n, s, res) = printfn "Problem #%i: %s = %A" n s res
 
    // What is the largest prime factor of the number 600851475143?
    let problem3 = 
        let result = Math.lpf 600851475143I
        (3, "largest prime factor of 600851475143", result)

    // Find the largest palindrome made from the product of two 3-digit numbers.
    let problem4 = 
        let isPalindrome (s:string) = 
            let chars = s.ToCharArray()
            let charsInv = chars |> Array.rev
            charsInv = chars
        
        let multiples = seq { for i=100 to 999 do for j=100 to 999 do yield i*j }
        multiples 
        |> Seq.filter(fun x -> isPalindrome(x.ToString()))
        |> Seq.toList |> List.sort  |> List.rev

    // What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
    let problem5 = 
        let divisible n x = seq { 1..n } |> Seq.forall(fun i -> x % i = 0)
        let matches = seq { 10000000..100000000 } |> Seq.filter(fun x -> x % 2 = 0) |> Seq.filter(divisible 20)
        1

    // Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
    let problem6 = 
        let sumSq = seq { for i=1 to 100 do yield i*i } |> Seq.sum
        let sqSum = (seq { 1..100 } |> Seq.sum) * (seq { 1..100 } |> Seq.sum)
        let diff = sqSum - sumSq
        1

    // There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.
    let problem9 = 
        let pySquares n = seq { for a = 1 to n do for b = 1 to n do for c = 1 to n do yield (a, b, c) }  |> Seq.filter(fun (a, b, c) -> a*a + b*b = c*c) |> Seq.filter(fun (a, b, c) -> a + b + c = n)
        printfn "%A" <| pySquares 1000