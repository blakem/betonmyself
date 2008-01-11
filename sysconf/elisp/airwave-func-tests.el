;; run with airwave-func-tests.sh
(setq test-count 110)
(require 'test-harness)
(require 'cperl-mode)

(message "1..%d => total: %d" test-count (* test-count 2))

(progn (testblock "test-harness.el framework")
  (with-temp-buffer
    (insert "abc")
    (backward-char 3)
    (Assert (looking-at "abc"))
    (goto-char 1)
    (Assert (looking-at "abc"))
    (Check-Error beginning-of-buffer
                 (backward-char 100))
    (Check-Error-Message error "Test .* Message"
                         (error "Test Error Message"))
    (Check-Message "blah"
                   (message "blah" nil)) ; will only display when run inside 
                                         ; emacs 
    )
  )

(progn (testblock "'self function")
  (with-cperl-buffer
    (self)
    (Assert (looking-at ")"))
    (goto-char (point-min))
    (Assert (looking-at "my ($self) = @_"))
    )
)


(progn (testblock "toggle if/unless")
  (with-cperl-buffer
    (insert "blah")
    (backward-char 2)
    (Check-Error-Message error "Not on postfix"
                         (airwave-toggle-if-unless))
    )
  
  (with-cperl-buffer
    (insert "$that if $this")
    (goto-char (point-min))
    (search-forward " if ")
    (backward-char 3)
    (Assert (looking-at "if "))
    (airwave-toggle-if-unless)
    (Assert (looking-at "($this)"))
    (goto-char (point-min))
    (Assert (looking-at "if ($this) {\n  $that;\n}"))
    )

  (with-cperl-buffer
    (insert "$that while $this")
    (goto-char (point-min))
    (search-forward " while ")
    (backward-char 6)
    (Assert (looking-at "while "))
    (airwave-toggle-if-unless)
    (Assert (looking-at "($this)"))
    (goto-char (point-min))
    (Assert (looking-at "while ($this) {\n  $that;\n}"))
    )

  (with-cperl-buffer
    (insert "$that for $this")
    (goto-char (point-min))
    (search-forward " for ")
    (backward-char 4)
    (Assert (looking-at "for "))
    (airwave-toggle-if-unless)
    (Assert (looking-at "($this)"))
    (goto-char (point-min))
    (Assert (looking-at "for ($this) {\n  $that;\n}"))
    )

  (with-cperl-buffer
    (insert "$that foreach $this")
    (goto-char (point-min))
    (search-forward " foreach ")
    (backward-char 8)
    (Assert (looking-at "foreach "))
    (airwave-toggle-if-unless)
    (Assert (looking-at "($this)"))
    (goto-char (point-min))
    (Assert (looking-at "foreach ($this) {\n  $that;\n}"))
    )

  (with-cperl-buffer
    (insert "$that unless $this")
    (goto-char (point-min))
    (search-forward " unless ")
    (backward-char 7)
    (Assert (looking-at "unless "))
    (airwave-toggle-if-unless)
    (Assert (looking-at "($this)"))
    (goto-char (point-min))
    (Assert (looking-at "unless ($this) {\n  $that;\n}"))
    )

  (with-cperl-buffer
    (insert "$that until $this")
    (goto-char (point-min))
    (search-forward " until ")
    (backward-char 6)
    (Assert (looking-at "until "))
    (airwave-toggle-if-unless)
    (Assert (looking-at "($this)"))
    (goto-char (point-min))
    (Assert (looking-at "until ($this) {\n  $that;\n}"))
    )

  (with-cperl-buffer
    (insert "$that while $this")
    (goto-char (point-min))
    (search-forward " while ")
    (backward-char 6)
    (Assert (looking-at "while "))
    (airwave-toggle-if-unless)
    (Assert (looking-at "($this)"))
    (goto-char (point-min))
    (Assert (looking-at "while ($this) {\n  $that;\n}"))
    )

  (with-cperl-buffer
    (insert "$that foreach $this, $foo, $bar")
    (goto-char (point-min))
    (search-forward " foreach ")
    (backward-char 8)
    (Assert (looking-at "foreach "))
    (airwave-toggle-if-unless)
    (Assert (looking-at "($this, $foo, $bar)"))
    (goto-char (point-min))
    (Assert (looking-at "foreach ($this, $foo, $bar) {\n  $that;\n}"))
    )

  (with-cperl-buffer
    (insert "{\n  $that if $this\n}")
    (goto-char (point-min))
    (search-forward " if ")
    (backward-char 3)
    (Assert (looking-at "if "))
    (airwave-toggle-if-unless)
    (Assert (looking-at "($this)"))
    (goto-char (point-min))
    (Assert (looking-at "{\n  if ($this) {\n    $that;\n  }\n}"))
    )

  (with-cperl-buffer
    (insert "$that\n  if $this\n")
    (goto-char (point-min))
    (search-forward " if ")
    (backward-char 3)
    (Assert (looking-at "if "))
    (airwave-toggle-if-unless)
    (Assert (looking-at "($this)"))
    (goto-char (point-min))
    (Assert (looking-at "if ($this) {\n  $that;\n}"))
    )

  (with-cperl-buffer
    (insert "$a and $b and $those and $that if $this\n")
    (goto-char (point-min))
    (search-forward " if ")
    (backward-char 3)
    (Assert (looking-at "if "))
    (airwave-toggle-if-unless)
    (Assert (looking-at "($this"))
    (goto-char (point-min))
    (Assert (looking-at "if ($this) {\n  $a and $b and $those and $that;\n}"))
    )

  (with-cperl-buffer
    (insert "$that if $this and $foobar\n")
    (goto-char (point-min))
    (search-forward " if ")
    (backward-char 3)
    (Assert (looking-at "if "))
    (airwave-toggle-if-unless)
    (Assert (looking-at "($this and $foobar"))
    (goto-char (point-min))
    (Assert (looking-at "if ($this and $foobar) {\n  $that;\n}"))
    )

  (with-cperl-buffer
    (insert "$that\n  and $foo\n    and $bar\n      if $this\n")
    (goto-char (point-min))
    (set-mark (point))
    (search-forward " if ")
    (backward-char 3)
    (Assert (looking-at "if "))
    (airwave-toggle-if-unless)
    (Assert (looking-at "($this)"))
    (goto-char (point-min))
    (Assert (looking-at "if ($this) {\n  $that\n    and $foo\n      and $bar;\n}"))
    )

  (with-cperl-buffer
    (insert "if ($this) {\n  $that;\n}")
    (goto-char (point-min))
    (search-forward "if ")
    (Assert (looking-at "($this"))
    (airwave-toggle-if-unless)
    (Assert (looking-at "if "))
    (goto-char (point-min))
    (Assert (looking-at "$that if $this;"))
    )
  )

(progn (testblock "vertical-horizontal lists")
  (with-cperl-buffer 
   (insert "{\n  my @a = (1,2,3);\n}")
   (goto-char (point-min))
   (search-forward "(")
   (backward-char 1)
   (Assert (looking-at "("))
   (airwave-toggle-vertical-horizontal-list)
   (Assert (looking-at ")"))
   (goto-char (point-min))
   (Assert (looking-at "{\n  my @a = (\n    1,\n    2,\n    3,\n  );\n}"))
   
   (search-forward ")")
   (backward-char 1)
   (airwave-toggle-vertical-horizontal-list)
   (Assert (looking-at "("))
   (goto-char (point-min))
   (Assert (looking-at "{\n  my @a = (1, 2, 3);\n}"))
   )
  
  (with-cperl-buffer 
   (insert "{\n  my @a = (1,  2,  3,,,);\n}")
   (goto-char (point-min))
   (search-forward "(")
   (backward-char 1)
   (Assert (looking-at "("))
   (airwave-toggle-vertical-horizontal-list)
   (Assert (looking-at ")"))
   (goto-char (point-min))
   (Assert (looking-at "{\n  my @a = (\n    1,\n    2,\n    3,\n  );\n}"))
   )

  (with-cperl-buffer 
   (insert "{\n  my @a = (  1,,  2,  3,,,);\n}")
   (goto-char (point-min))
   (search-forward "(")
   (backward-char 1)
   (Assert (looking-at "("))
   (airwave-toggle-vertical-horizontal-list)
   (Assert (looking-at ")"))
   (goto-char (point-min))
   (Assert (looking-at "{\n  my @a = (\n    1,\n    ,\n    2,\n    3,\n  );\n}"))
   
   (search-forward ")")
   (backward-char 1)
   (airwave-toggle-vertical-horizontal-list)
   (Assert (looking-at "("))
   (goto-char (point-min))
   (Assert (looking-at "{\n  my @a = (1, , 2, 3);\n}"))
   )
  
  (with-cperl-buffer 
   (insert "{\n  my $x = {1,2,3,4};\n}")
   (goto-char (point-min))
   (search-forward "{1")
   (backward-char 2)
   (Assert (looking-at "{1"))
   (airwave-toggle-vertical-horizontal-list)
   (Assert (looking-at "}"))
   (goto-char (point-min))
   (Assert (looking-at "{\n  my $x = {\n    1,\n    2,\n    3,\n    4,\n  };\n}"))
   
   (search-forward "}")
   (backward-char 1)
   (airwave-toggle-vertical-horizontal-list)
   (Assert (looking-at "{"))
   (goto-char (point-min))
   (Assert (looking-at "{\n  my $x = {1, 2, 3, 4};\n}"))
   )
  
  (with-cperl-buffer 
   (insert "{\n  my $x = {1 => 2, 3 => 4};\n}")
   (goto-char (point-min))
   (search-forward "{1")
   (backward-char 2)
   (Assert (looking-at "{1"))
   (airwave-toggle-vertical-horizontal-list)
   (Assert (looking-at "}"))
   (goto-char (point-min))
   (Assert (looking-at "{\n  my $x = {\n    1 => 2,\n    3 =\> 4,\n  };\n}"))
   
   (search-forward "}")
   (backward-char 1)
   (airwave-toggle-vertical-horizontal-list)
   (Assert (looking-at "{"))
   (goto-char (point-min))
   (Assert (looking-at "{\n  my $x = {1 => 2, 3 => 4};\n}"))
   )
  
  (with-cperl-buffer 
   (insert "my $x = {1,2,3,4};")
   (goto-char (point-min))
   (search-forward "4}")
   (backward-char 1)
   (Assert (looking-at "}"))
   (airwave-toggle-vertical-horizontal-list)
   (Assert (looking-at "{"))
   (goto-char (point-min))
   (Assert (looking-at "my $x = {1, 2, 3, 4}"))
   )  
  
  (with-cperl-buffer 
   (insert "{\n  my @a = qw(a    b c   d e  );\n}")
   (goto-char (point-min))
   (search-forward "(")
   (backward-char 1)
   (Assert (looking-at "("))
   (airwave-toggle-vertical-horizontal-list)
   (Assert (looking-at ")"))
   (goto-char (point-min))
   (Assert (looking-at "{\n  my @a = qw(\n    a\n    b\n    c\n    d\n    e\n  );\n}"))
   
   (search-forward ")")
   (backward-char 1)
   (airwave-toggle-vertical-horizontal-list)
   (Assert (looking-at "("))
   (goto-char (point-min))
   (Assert (looking-at "{\n  my @a = qw(a b c d e);\n}"))
   )
)

(progn (testblock "toggle-spaces-in-sexp")
  (with-cperl-buffer
   (insert "$x = ($a->(), $b{}, $c[{()}], $d()())")
   (goto-char (point-min))
   (search-forward "(")
   (backward-char 1)
   (airwave-toggle-spaces-in-sexp)
   (Assert (looking-at "("))
   (Assert (looking-at "( $a->(), $b{}, $c\\[{()}\\], $d()() )"))
   (airwave-toggle-spaces-in-sexp)
   (Assert (looking-at "($a->(), $b{}, $c\\[{()}\\], $d()())"))
   )

  (with-cperl-buffer
   (insert "$x = (1, 2, 3, 4)")
   (goto-char (point-min))
   (search-forward "(")
   (backward-char 1)
   (airwave-toggle-spaces-in-sexp)
   (Assert (looking-at "( 1, 2, 3, 4 )"))
   (airwave-toggle-spaces-in-sexp)
   (Assert (looking-at "(1, 2, 3, 4)"))

   (forward-char 1)
   (insert " ")
   (backward-char 2)
   (airwave-toggle-spaces-in-sexp)
   (Assert (looking-at "(1, 2, 3, 4)"))
   (search-forward ")")
   (backward-char 1)
   (insert " ")
   (airwave-toggle-spaces-in-sexp)
   (search-backward "(")
   (Assert (looking-at "(1, 2, 3, 4)"))

   (forward-char 1)
   (insert " ")
   (search-forward ")")
   (backward-char 1)
   (airwave-toggle-spaces-in-sexp)
   (search-backward "(")
   (Assert (looking-at "( 1, 2, 3, 4 )"))

   (forward-char 1)
   (delete-char 1)
   (backward-char 1)
   (airwave-toggle-spaces-in-sexp)
   (Assert (looking-at "( 1, 2, 3, 4 )"))
   )


  (with-cperl-buffer
   (insert "$x = {1, 2, 3, 4}")
   (goto-char (point-min))
   (search-forward "{")
   (backward-char 1)
   (airwave-toggle-spaces-in-sexp)
   (Assert (looking-at "{ 1, 2, 3, 4 }"))
   (airwave-toggle-spaces-in-sexp)
   (Assert (looking-at "{1, 2, 3, 4}"))

   (forward-char 1)
   (insert " ")
   (backward-char 2)
   (airwave-toggle-spaces-in-sexp)
   (Assert (looking-at "{1, 2, 3, 4}"))
   (search-forward "}")
   (backward-char 1)
   (insert " ")
   (airwave-toggle-spaces-in-sexp)
   (search-backward "{")
   (Assert (looking-at "{1, 2, 3, 4}"))

   (forward-char 1)
   (insert " ")
   (search-forward "}")
   (backward-char 1)
   (airwave-toggle-spaces-in-sexp)
   (search-backward "{")
   (Assert (looking-at "{ 1, 2, 3, 4 }"))

   (forward-char 1)
   (delete-char 1)
   (backward-char 1)
   (airwave-toggle-spaces-in-sexp)
   (Assert (looking-at "{ 1, 2, 3, 4 }"))
   )
  )

(progn (testblock "kill-whole-word")
  (with-cperl-buffer
   (insert "a very_long_word followed by some more")
   (goto-char 5)
   (airwave-kill-whole-word)
   (goto-char (point-min))
   (Assert (looking-at "a _long_word followed"))
   (forward-char 1)
   (airwave-kill-whole-word)
   (goto-char (point-min))
   (Assert (looking-at "a _long_word followed"))
   (forward-char 2)
   (airwave-kill-whole-word)
   (goto-char (point-min))
   (Assert (looking-at "a long_word followed"))
   )
  )

(progn (testblock "remove-braces")
  (with-cperl-buffer
   (insert "$x->{autoloaded} ")
   (goto-char 7)
   (airwave-remove-braces)
   (goto-char (point-min))
   (Assert (looking-at "$x->autoloaded "))
   )
  )

(progn (testblock "airwave-find-perl-variable")
  (with-cperl-buffer  
     (insert "my $scalar = 74")
     (goto-char 6)
     (Assert (string= (airwave-find-perl-variable) "$scalar"))
     (clear-buffer)

     (insert "my %hash = (dog => 1)")
     (goto-char 6)
     (Assert (string= (airwave-find-perl-variable) "%hash"))
     (clear-buffer)

     (insert "my @array = (dog => 1)")
     (goto-char 6)
     (Assert (string= (airwave-find-perl-variable) "@array"))
     (clear-buffer)

     (insert "my $array[10] = 5")
     (goto-char 6)
     (Assert (string= (airwave-find-perl-variable) "$array[10]"))
     (clear-buffer)

     (insert "my $hash{key} = 10")
     (goto-char 6)
     (Assert (string= (airwave-find-perl-variable) "$hash{key}"))
     (clear-buffer)

     (insert "my $hash{key} = 10")
     (goto-char 11)
     (Assert (string= (airwave-find-perl-variable) "$hash{key}"))
     (clear-buffer)

     (insert "my $object->autoload = 5")
     (goto-char 6)
     (Assert (string= (airwave-find-perl-variable) "$object->autoload"))
     (clear-buffer)

     (insert "my $object->autoload = 5")
     (goto-char 15)
     (Assert (string= (airwave-find-perl-variable) "$object->autoload"))
     (clear-buffer)

     (insert "my $object->destroy() = 5")
     (goto-char 6)
     (Assert (not (airwave-find-perl-variable)))
     (clear-buffer)

     (insert "my $object->destroy() = 5")
     (goto-char 15)
     (Assert (not (airwave-find-perl-variable)))
     (clear-buffer)
     )
  )

(message "1..%d => total: %d" test-count (* test-count 2))

