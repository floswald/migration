module E_tensors
export FinalV,expect_y
# Generated tensor:
# FinalV log(A_[a] + H_[h] * ( P_[P] + p_[p,j] ) ) | a,h,P,j,p
function FinalV(A_,H_,P_,p_)
nh = size(H_)[1]
na = size(A_)[1]
nj = size(p_)[2]
np = size(p_)[1]
nP = size(P_)[1]
@inbounds begin
Res = zeros(na,nh,nP,nj,np)
for a in 1:na
 for h in 1:nh
  for P in 1:nP
   for j in 1:nj
    for p in 1:np
     Res[a,h,P,j,p]= 0
     Res[a,h,P,j,p] =  Res[a,h,P,j,p].+log(A_[a].+H_[h].*P_[P].+p_[p + np * (j-1)])
    end
   end
  end
 end
end
end
return Res
end

# Generated tensor:
# expect_y V[a,y1,p1,z,t] * Gy[y,y1] * Gp[p,p1] | a,y,p,z,t
function expect_y(Gp,Gy,V)
np = size(V)[3]
na = size(V)[1]
nz = size(V)[4]
nt = size(V)[5]
ny = size(V)[2]
@inbounds begin
Res = zeros(na,ny,np,nz,nt)
for a in 1:na
 for y in 1:ny
  for p in 1:np
   for z in 1:nz
    for t in 1:nt
     Res[a,y,p,z,t]= 0
     for y1 in 1:ny
      for p1 in 1:np
       Res[a,y,p,z,t] =  Res[a,y,p,z,t].+V[a + na * (y1 + ny * (p1 + np * (z + nz * (t-1)-1)-1)-1)].*Gy[y + ny * (y1-1)].*Gp[p + np * (p1-1)]
      end
     end
    end
   end
  end
 end
end
end
return Res
end

end
