

# AsciiEDoc - EDoc extension for generating HTML or Github-flavored Markdown from AsciiDoc sources #

Copyright (c) 2011-2015 by Joseph Wayne Norton

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).
<p>AsciiEdoc is an Erlang application that integrates AsciiDoc with EDoc
or EDoc with EDown.  This may sound a little crazy at first but the
primary reasons are:</p>
<ul>
<li>
<p>
Dislike EDoc's primitive wiki support.
</p>
</li>
<li>
<p>
<strong>Really</strong> dislike having to use XHTML for EDoc purposes.
</p>
</li>
<li>
<p>
Not crazy about Markdown either. Markdown is helpful only in one
  sense - GitHub integration.
</p>
</li>
<li>
<p>
AsciiDoc is simple, easy to use, friendly to humans, and yet
  expressive "enough" to produce high-quality documentation.
</p>
</li>
</ul>
<p>This documentation flow is a work-in-progress and the error reporting
is fragile when something breaks.  Nevertheless, this documentation
flow may be of interest to others.</p>
<p>For sample AsciiDoc-annotated overview.edoc and Erlang source files,
see UBF and UBF-related repositories on GitHub
(<a href="https://github.com/ubf/ubf">https://github.com/ubf/ubf</a>) for details.</p>
<p><em>This repository is experimental in nature - use at your own risk and
please contribute if you find AsciiEDoc useful.</em></p>

<h2 id="_quick_start_recipe">Quick Start Recipe</h2>

<p>To download, build, and test the asciiedoc application in one shot,
please follow this recipe:</p>


<pre><code>$ mkdir working-directory-name
$ cd working-directory-name
$ git clone https://github.com/norton/asciiedoc.git asciiedoc
$ cd asciiedoc
$ make deps clean compile doc</code></pre>




<h2 id="_documentation">Documentation</h2>


<h3 id="_where_should_i_start">Where should I start?</h3>
<p>This README is the only bit of documentation right now.</p>


<h3 id="_what_is_edoc">What is EDoc?</h3>
<p>EDoc is the Erlang program documentation generator. Inspired by the
Javadoc tool for the Java programming language, EDoc is adapted to the
conventions of the Erlang world, and has several features not found in
Javadoc.</p>
<p>See <a href="http://www.erlang.org/doc/apps/edoc/index.html">http://www.erlang.org/doc/apps/edoc/index.html</a> for further
details.</p>


<h3 id="_what_is_edown">What is EDown?</h3>
<p>EDown is an Erlang application that generates "More-or-less readable"
Markdown from Erlang program documentation.</p>
<p>See <a href="https://github.com/esl/edown">https://github.com/esl/edown</a> for further details.</p>


<h3 id="_what_is_asciidoc">What is AsciiDoc?</h3>
<p>AsciiDoc is a text document format for writing notes, documentation,
articles, books, ebooks, slideshows, web pages, man pages and blogs.
AsciiDoc files can be translated to many formats including HTML, PDF,
EPUB, man page.</p>
<p>AsciiDoc is highly configurable: both the AsciiDoc source file syntax
and the backend output markups (which can be almost any type of
SGML/XML markup) can be customized and extended by the user.</p>
<p>AsciiDoc is free software and is licenced under the terms of the <em>GNU
General Public License version 2</em> (GPLv2).</p>
<p>See <a href="http://www.methods.co.nz/asciidoc/index.html">http://www.methods.co.nz/asciidoc/index.html</a> for further details.</p>





## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/norton/asciiedoc/blob/master/doc/asciiedoc_doclet.md" class="module">asciiedoc_doclet</a></td></tr>
<tr><td><a href="https://github.com/norton/asciiedoc/blob/master/doc/asciiedoc_lib.md" class="module">asciiedoc_lib</a></td></tr>
<tr><td><a href="https://github.com/norton/asciiedoc/blob/master/doc/asciiedown_doclet.md" class="module">asciiedown_doclet</a></td></tr></table>

