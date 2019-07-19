float [] solveJacobian (float px, float py, float pw, float w1,float w2, float w3, float pxDiff , float pyDiff , float pwDiff ) {
        float new_angles [] = { 0.0, 0.0, 0.0 };
        float ve[] = {px - pxDiff, py - pyDiff, pw - pwDiff }; // velocity of pe
        float J [][] = new float [3][3]; // Jacobian
        J [0][0] = -L1 * sin(w1) - L2 * sin(w1 + w2) - L3 * sin(w1 + w2 + w3);
        J [0][1] = -L2 * sin(w1 + w2) - L3 * sin(w1 + w2 + w3);
        J [0][2] = -L3 * sin(w1 + w2 + w3);
        J [1][0] = L1 * cos(w1) + L2 * cos(w1 + w2) + L3 * cos(w1 + w2 + w3);
        J [1][1] = L2 * cos(w1 + w2) + L3 * cos(w1 + w2 + w3);
        J [1][2] = L3 * cos(w1 + w2 + w3);
        J [2][0] = 1;
        J [2][1] = 1;
        J [2][2] = 1;
        float Jinv [][] = inv3 ( J ) ; // compute J-1
        float qDiff [] = multiply3x1 ( Jinv , νe ); // angle changes
        new_angles [0] = w1 + Diffq[0];
        new_angles [1] = w2 + Diffq[1];
        new_angles [2] = w3 + Diffq[2];
        return ( new_angles );
        } // end of solveJacobian ()