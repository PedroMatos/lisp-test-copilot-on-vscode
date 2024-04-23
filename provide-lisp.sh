# Install SBCL (Lisp) if not available.
# ------------------------------------------------------------------------------

if [ -z "$(command -v sbcl)" ]; then
    echo "Updating package database..."
    sudo apt-get update
    echo "Installing SBCL (Lisp)..."    
    sudo apt-get install -y sbcl
else
    echo "SBCL (Lisp) is already installed."
fi
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(ql:add-to-init-file)' --quit
