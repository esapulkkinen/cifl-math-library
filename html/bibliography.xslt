<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="/">
    <html>
    <head>
     <title>Bibliography</title>
    </head>
    <body>
    <h1>Bibliography</h1>
    <div id="structured">
    <table width="500" padding="5px" border-collapse="collapse">
      <xsl:for-each select="bibliography/creativeWork">
        <xsl:sort select="summary"/>
        <tr itemscope="itemscope" itemtype="http://schema.org/CreativeWork">
          <td>
            <details>
              <summary itemprop="name"><b><xsl:value-of select="summary"/></b></summary>
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
    <script type="text/javascript" src="http://github.io/esapulkkinen/cifl-math-library/copyright.js">
    </script>
    </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
