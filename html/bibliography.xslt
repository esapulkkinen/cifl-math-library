<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="/">
    <html>
    <head>
      <title>Bibliography</title>
      <style>
        body { background-color: white; color: black; }
        nav tr { display: grid; background-color: lightblue; color: black; }
        nav td { display: grid; background-color: lightblue; color: black; }
        div { background-color: lightgrey; color: black; }
        table { table-layout: auto; width:100% border: 1px solid black; border-collapse: collapse; }
        th { display: flex; color: red; width: 30%;  text-align: right; }
        td { display: flex; width: 70%; text-align: left; }
      </style>
    </head>
    <body>
      <nav>
        <table>
          <tr>
            <td><a href="https://esapulkkinen.github.io/cifl-math-library/">Contents</a></td>
            <td><a href="https://github.com/esapulkkinen/cifl-math-library/">Repository</a></td>
            <td><a href="https://esapulkkinen.github.io/cifl-math-library/doc-index.html">Index</a></td>
          </tr>
        </table>
      </nav>
      
    <h1>Bibliography</h1>
    <div id="structured">
      <table frame="vsides" padding="5px" border-collapse="collapse">
      <xsl:for-each select="bibliography/creativeWork">
        <xsl:sort select="summary"/>
        <tr itemscope="itemscope" itemtype="http://schema.org/CreativeWork">
          <td colspan="2">
            <details>
              <summary><xsl:value-of select="summary"/></summary>
              <xsl:for-each select="field">
                <xsl:sort select="attribute::name"/>
                <div>
                  <xsl:attribute name="itemprop">
                    <xsl:value-of select="attribute::name"/>
                  </xsl:attribute>
                  <xsl:apply-templates/>
                </div>
              </xsl:for-each>
            </details>
          </td>
        </tr>
      </xsl:for-each>
    </table>
    </div>
    <script type="text/javascript"
      src="https://esapulkkinen.github.io/cifl-math-library/copyright.js">
    </script>
    </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
