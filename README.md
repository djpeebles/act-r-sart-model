# act-r-sart-model
An ACT-R model of the Sustained Attention To Response (SART) task.  The model was originally reported in Peebles, D., & Bothell, D. (2004). Modelling performance in the Sustained Attention to Response Task. In M. Lovett, C. D. Schunn, C. Lebiere & P. Munro (Eds.). Proceedings of the Sixth International Conference on Cognitive Modeling. Mahwah , NJ : Lawrence Erlbaum.

This version has been updated to work with ACT-R 7

Instructions for use.

1. Extract the actr7.zip file containing the ACT-R code.
2. In the resulting actr7 folder, create a folder called 'models'.
3. In the 'models' folder place the lisp file: sart.lisp.
4. In your lisp, load the 'load-act-r.lisp' file.
5. Load the 'sart.lisp' file.
6. Run the model using the function (collect-data 100) which will run it for 100 simulated participants.  The output you'll get will hopefully look like the screenshot.  The first bit is a list of the RTs, and errors for each participant.  The second bit is the average over all the participants.  The ordering is the same as in the Peebles and Bothell (2004) paper.
 
To see the activity of the various buffers during a trial change the :v and :buffer-trace parameters from nil to t.

