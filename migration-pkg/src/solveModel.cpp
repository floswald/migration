
// computation outline


// map objects to tensors


// ///////////////////////////////
// objects to compute for solution
// ///////////////////////////////
  
// v(a,y,p,h,j,k,t) is the choice specific value function
// v(a,y,p,h,j,k,t) = max( v0(a,y,p,h,j,k,t), v1(a,y,p,h,j,k,t) )
//
// vi is value of choosign house size i:
// vi(a,y,p,h,j,k,t)    = max_a' U(Ci(a,y,p,h,j,k,t,a')) + beta E[ w(a',y',p',hi,k,t+1) ]
//
// w is future value function in logit form
// w(a',y',p',h',k,t+1) = gamma + ln( sum_j ( exp( v(a',y',p',h',k,j,t+1) ) ) )
//
// Ci is constumption given state and savings choice a'
// Ui is corresponding utility
// Ui(a,y,p,h,j,k,t,a') = u(a,y,p,h,j,k,t,h'=i) - a'
  
  
// ///////////////////////////////
// Results to collect from solution
// ///////////////////////////////
  
// choice specific value funs
// v(a,y,p,h,j,k,t)
//
// optimal savings for each house choice
// astar_i(a,y,p,h,j,k,t) = maxIndex( U(Ci(a,y,p,h,j,k,t,a')) + beta E[ w(a',y',p',hi,k,t+1) ] , lastDim )
//
// optimal cons for each house choice
// cstar_i(a,y,p,h,j,k,t) = Ci( astar_i(a,y,p,h,j,k,t) )
//
// optimal house choice
// hstar(a,y,p,h,j,k,t) = maxIndex( v0(a,y,p,h,j,k,t), v1(a,y,p,h,j,k,t) )
//
// conditional choice probabilities
// (only needed in maximum likelihood estimation)
// rho(a,y,p,h,j,k,t) = exp( gamma + v(a,y,p,h,j,k,t) - w(a,y,p,h,j,t) )



// ///////////
// computation
// ///////////
  
// time loop

// computation of values

// end time
 

// //////////
// simulation
// //////////
  
// given v(a,y,p,h,j,k,t) and random sequences for {y_it,p_t,xi_ijt}, get
// 
// 1) discrete location choice:
// loc_choice(a,y_it,p_,j,t) = maxIndex( v(a,y_it,p_,j,k,t) + xi_ijt, dimk )
//
// 2) for each it, simulate forward
// goto_it = loc_choice(a,y_it,p_,j,t)
// hsim(a,y_it,p_,j,goto_it,t) = hstar(a,y_it,p_,j,goto_it,t)
// asim(a,y_it,p_,j,loc_choice(a,y_it,p_,j,t),t) = astar_hsim(a,y_it,p_,j,goto_it,t)
// csim(a,y_it,p_,j,k,t) = csim_hsim(asim_i)


