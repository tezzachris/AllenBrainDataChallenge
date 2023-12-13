

%!!To use the following script it is necessary to save the function "loglik_kalman" below in a separate file!!

%Import data from the data folder

yt = dfa.calcium;
yt = yt(2:end);
zt = dfa.state;
zt = zt(2:end);
zt(zt==2) = 0; %non active

%Some initial values for the paramters, ideally should give a grid and iterate
alpha0 = 0.0059;
alpha1 = 1;
beta1 = 0.87;
beta2 = 0.059;
vary = 0.001;
varx = 0.1;
theta = [alpha0,alpha1,beta1,beta2,vary,varx];
options = optimoptions('fminunc','Display','none','MaxFunctionEvaluations',5000);

%Perfom the optimization 
[param] = fminunc( 'loglik_kalman' , theta , options, yt, zt); 

%Obtain the desired output i.e. the filtered calcium series
[~,~,Xt] = loglik_kalman(param, yt, zt);



%Function 

%Inputs

%theta: [double 6x1] is the parameters vector 
%yt: [double nx1] the observed calcium trace
%zt: [double nx1] the activation states (i.e. 1's and 0's array, where 1 is active state) -- output of HMM model

%Outputs

%LL: value of the (negative) log-likelihood , negative because we will minimize it
%lls: [double nx1] are the log-likelihoods at each time t
%Xt: [double nx1] the filtered calcium trace

function [LL,lls,Xt] = loglik_kalman(theta, yt, zt)

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
    LL = -LL; %Note: we pass the negative value since the optmizer minimizes only
end
