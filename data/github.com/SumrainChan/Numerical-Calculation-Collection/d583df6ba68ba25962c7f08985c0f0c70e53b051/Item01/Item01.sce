A = zeros(1000, 1000)
for i=1:1000
    A(i, i) = i
    if i+1 <= 1000
        A(i, i+1) = 0.5
        A(i+1, i) = 0.5
    end
    if i+2 <= 1000
        A(i, i+2) = 0.5
        A(i+2, i) = 0.5
    end
end

b = ones(1000, 1)
x0 = zeros(1000, 1)
D = diag(diag(A))
U = triu(A, 1)
L = tril(A, -1)

// Jacobi
xk = x0
for step=1:15
    xk_next = inv(D) * (b - (L + U) * xk) 
    xk = xk_next
end
ans_jcb = xk

// Gauss-Seidel
xk = x0
for step=1:15
    xk_next = inv(L + D) * (-U * xk + b)
    xk = xk_next
end
ans_gs = xk

// SOR w=1.1
w = 1.1
xk = x0
for step=1:15
    xk_next = inv(w * L + D) * ((1 - w) * D * xk - w * U * xk) + w * inv(D + w * L) * b
    xk = xk_next
end
ans_sor = xk

// Conjugate Gradient
d0 = b - A * x0
r0 = d0
dk = d0
rk = r0
xk = x0
for step=1:15
    if rk == 0
        break
    end
    alphak = (rk' * rk) / (dk' * A * dk)
    xk_next = xk + alphak * dk
    rk_next = rk - alphak * A * dk
    betak = (rk_next' * rk_next) / (rk' * rk)
    dk_next = rk_next + betak * dk
    dk = dk_next
    rk = rk_next
    xk = xk_next
end
ans_cg = xk

// Conjugate Gradient with jcb preconditioner
M = D
r0 = b - A * x0
d0 = inv(M) * r0
z0 = d0
dk = d0
zk = z0
rk = r0
xk = x0
for step=1:15
    if rk == 0
        break
    end
    alphak = (rk' * zk) / (dk' * A *dk)
    xk_next = xk + alphak * dk
    rk_next = rk - alphak * A * dk
    zk_next = inv(M) * rk_next
    betak = (rk_next' * zk_next) / (rk' * zk)
    dk_next = zk_next + betak * dk
    dk = dk_next
    zk = zk_next
    rk = rk_next
    xk = xk_next
end
ans_cgp = xk

