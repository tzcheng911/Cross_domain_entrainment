function [C, rC] = temporalContrast(phi, ioi, p)
%Formula for Temporal Contrast (C) a la McAuley & Jones (2003)

if mod( phi + ioi/p, 1) > 0.5
    C = mod( phi + ioi/p, 1) - 1;
else
    C = mod( phi + ioi/p, 1);
end

rC = phi+ioi/p;