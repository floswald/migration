module E_tensors
export test,T_Evbar,T_FinalV,T_Final
# Generated tensor:
# test V[a,b,c] * B[b,c] | a,b,c
function test(B,V)
na = size(V)[1]
nb = size(V)[2]
nc = size(V)[3]
@inbounds begin
Res = zeros(na,nb,nc)
for a = 1:na
 for b = 1:nb
  for c = 1:nc
   Res[a,b,c]= 0
   Res[a,b,c] =  Res[a,b,c].+V[a + na * (b + nb * (c-1)-1)].*B[b + nb * (c-1)]
  end
 end
end
end
return Res
end

# Generated tensor:
# T_Evbar V[a,h,y1,p1,P1,z1,tau,j,age] * Gz[z,z1,j] * Gp[p,p1,j] * Gy[y,y1,j] * GP[P,P1] | a,h,y,p,P,z,tau,j,age
function T_Evbar(GP,Gp,Gy,Gz,V)
ntau = size(V)[7]
nh = size(V)[2]
nj = size(V)[8]
na = size(V)[1]
ny = size(V)[3]
nz = size(V)[6]
nage = size(V)[9]
nP = size(V)[5]
np = size(V)[4]
@inbounds begin
Res = zeros(na,nh,ny,np,nP,nz,ntau,nj,nage)
for a = 1:na
 for h = 1:nh
  for y = 1:ny
   for p = 1:np
    for P = 1:nP
     for z = 1:nz
      for tau = 1:ntau
       for j = 1:nj
        for age = 1:nage
         Res[a,h,y,p,P,z,tau,j,age]= 0
         for y1 = 1:ny
          for P1 = 1:nP
           for p1 = 1:np
            for z1 = 1:nz
             Res[a,h,y,p,P,z,tau,j,age] =  Res[a,h,y,p,P,z,tau,j,age].+V[a + na * (h + nh * (y1 + ny * (p1 + np * (P1 + nP * (z1 + nz * (tau + ntau * (j + nj * (age-1)-1)-1)-1)-1)-1)-1)-1)].*Gz[z + nz * (z1 + nz * (j-1)-1)].*Gp[p + np * (p1 + np * (j-1)-1)].*Gy[y + ny * (y1 + ny * (j-1)-1)].*GP[P + nP * (P1-1)]
            end
           end
          end
         end
        end
       end
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

# Generated tensor:
# T_FinalV log(A_[a] + (H_[h] * P_[P,p,j] ) ) | a,h,P,j,p
function T_FinalV(A_,H_,P_)
nh = size(H_)[1]
na = size(A_)[1]
nj = size(P_)[3]
np = size(P_)[2]
nP = size(P_)[1]
@inbounds begin
Res = zeros(na,nh,nP,nj,np)
for a = 1:na
 for h = 1:nh
  for P = 1:nP
   for j = 1:nj
    for p = 1:np
     Res[a,h,P,j,p]= 0
     Res[a,h,P,j,p] =  Res[a,h,P,j,p].+log((A_[a].+H_[h].*P_[P + nP * (p + np * (j-1)-1)]))
    end
   end
  end
 end
end
end
return Res
end

# Generated tensor:
# T_Final V[a,h,P1,p1,j] * GP[P,P1] * Gp[p,p1,j] | a,h,P,p,j
function T_Final(GP,Gp,V)
np = size(V)[4]
na = size(V)[1]
nP = size(V)[3]
nh = size(V)[2]
nj = size(V)[5]
@inbounds begin
Res = zeros(na,nh,nP,np,nj)
for a = 1:na
 for h = 1:nh
  for P = 1:nP
   for p = 1:np
    for j = 1:nj
     Res[a,h,P,p,j]= 0
     for p1 = 1:np
      for P1 = 1:nP
       Res[a,h,P,p,j] =  Res[a,h,P,p,j].+V[a + na * (h + nh * (P1 + nP * (p1 + np * (j-1)-1)-1)-1)].*GP[P + nP * (P1-1)].*Gp[p + np * (p1 + np * (j-1)-1)]
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
