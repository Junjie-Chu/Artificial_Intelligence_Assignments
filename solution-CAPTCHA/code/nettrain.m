%train CNN
function net = nettrain(X,Y)
    layers = [
    imageInputLayer([64 64 1])
    
    convolution2dLayer(3,8,'Padding','same')
    batchNormalizationLayer
    reluLayer
    
    maxPooling2dLayer(2,'Stride',2)
    
    convolution2dLayer(3,16,'Padding','same')
    batchNormalizationLayer
    reluLayer
    
    maxPooling2dLayer(2,'Stride',2)
    
    convolution2dLayer(3,32,'Padding','same')
    batchNormalizationLayer
    reluLayer
    
    fullyConnectedLayer(3)
    softmaxLayer
    classificationLayer];

    options = trainingOptions('sgdm', ...
    'MaxEpochs',30,...
    'InitialLearnRate',1e-2, ...
    'Verbose',false, ...
    'Plots','training-progress');

    net = trainNetwork(X,Y,layers,options);
    save net;
end