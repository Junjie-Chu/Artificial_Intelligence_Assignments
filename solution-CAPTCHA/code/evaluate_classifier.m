close all, clear all; % Close all figures, and clear all variables

%% Load the labels
true_labels = importdata('labels.txt');

%% Load or set the parameters for your classifier.
% For instance, if you choose to save your parameters as an .mat-file
% you can load them using the load(filename) function.
% You are allowed to have more/less than 2 parameters.

%% Using the net got from the privious training
load net;

%% If there is no net before, use the code to train
% [X_train,Y_train,X_test,Y_test] = buildtrainset(1000,true);
% net = nettrain(X_train,Y_train);
% 
% Y_predict=classify(net,X_test)';
% accuracy = sum(Y_predict == Y_test)/numel(Y_test);

%% Evaluate the classifier
tic; % Start the timer
my_labels = zeros(size(true_labels));
N = size(true_labels,1);
for k = 1:N
    im = imread(sprintf('imagedata/train_%04d.png', k));
    my_labels(k,:) = my_classifier(im, net);
end
fprintf('\n\nAverage precision: \n');
fprintf('%f\n\n',mean(sum(abs(true_labels - my_labels),2)==0));
toc; % Stop the timer

