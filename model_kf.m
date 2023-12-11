function [LL,lls,Xt] = model_kf(theta, yt, zt)

%num_observations
n = size(yt,1);

%import parameters
alpha0 = theta(1);
alpha1 = theta(2);
beta1 = theta(3);
beta2 = theta(4);
vary = theta(5);
varx = theta(6);

%initialize variables
Xt0 = mean(yt);
zt0 = zt(1);
Pt = eye(1,1);
Xt = zeros(n,1);
Z = alpha1;
lls = zeros(n,1);

    for i = 1 : n 
        if i > 1
            Xt0 = Xt(i-1); zt0 = zt(i);
        end
        Xpred =  beta1*Xt0 + beta2*zt0; 

        %Prediction equations
        ypred = alpha0 +  Z * Xpred ;
        Ppred = beta1^2 * Pt + varx ;

        %Prediction error
        vt = yt(i)  - ypred ;
        Ft = Z * Ppred * Z' + vary ;
    
        %Updating equations
        Kt = (Ppred * Z')/(Ft);
        Xt(i) = Xpred + (Kt * vt')';  
        Pt =  Ppred - Kt*Z*Ppred;
        lls(i) = - 0.5*( log(2*pi) + log( det(Ft) ) +  (vt / Ft * vt') ); %vt needs to be column vector
    end
    LL = sum(lls);
    lls = -lls;
    LL = -LL;
end
