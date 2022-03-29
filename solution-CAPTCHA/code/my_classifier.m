function S = my_classifier(im, net)
%This classifier is not state of the art... but should give you an idea of
%the format we expect to make it easy to keep track of your scores. Input
%is the image and different parameters. Output is a 1 x 3 vector of the 
%three numbers in the image
%
%This baseline classifier tries to guess... so should score about (3^3)^-1
%on average, approx. a 4% chance of guessing the correct answer. 
    cutim = cutting(im,false);
    S1 = classify(net,cutim{1});
    S2 = classify(net,cutim{2});
    S3 = classify(net,cutim{3});
    S = [S1,S2,S3];
    S = double(string(S));

end

