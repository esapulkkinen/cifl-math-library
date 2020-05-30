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
        table { table-layout: auto; width:100% border-collapse: collapse; }
        th { display: flex; color: red; width: 200; text-align: right; }
        td { display: flex; text-align: left; }
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
    <div id="structured" itemscope="itemscope" itemtype="http://schema.org/APIReference" itemid="http://esapulkkinen.github.io/cifl-math-library/">
      <table frame="vsides" padding="5px" border-collapse="collapse">
      <xsl:for-each select="bibliography/CreativeWork|bibliography/Article|bibliography/Report|bibliography/Book|bibliography/ScholarlyArticle|bibliography/BlogPosting|bibliography/TechArticle|bibliography/DigitalDocument|bibliography/PublicationIssue|bibliography/SoftwareApplication|bibliography/WebPage|bibliography/WebSite|bibliography/SoftwareSourceCode|bibliography/Comment|bibliography/QAPage|bibliography/BlogPosting|bibliography/DigitalDocument">
        <xsl:sort select="summary"/>
        <tr itemprop="citation" itemscope="itemscope">
          <xsl:attribute name="itemtype">http://schema.org/<xsl:value-of select="local-name()"/></xsl:attribute>
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
