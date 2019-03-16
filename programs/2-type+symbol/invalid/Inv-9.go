package main

//Invalid 
//Checks if literal struct type of function argument equates to defnied type of same struct type (fails )

func f(pt struct{
    p struct{
        x, y int
    }
    t int
}) int {
    return pt.p.x + pt.p.y + pt.t
}

func main() {
    type point struct{
        x, y int
    }

    type point_time struct{
        p point
        t int
    }

    var pt point_time
     
    f(pt)
}

