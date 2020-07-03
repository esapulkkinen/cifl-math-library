<?xml version="1.0"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html" version="5" doctype-system="about:legacy-compat"/>
  <xsl:template match="/">

    <html>
    <head>
      <title>Bibliography</title>
      <style>
        .noselect { user-select: none; webkit-user-select: none;
        khtml-user-select: none; moz-user-select: none; ms-user-select: none; }
        body { background-color: white; color: black; }
        nav tr { display: grid; background-color: lightblue; color: black; }
        nav th { display: grid; background-color: lightblue; color: black; }
        nav td { display: grid; background-color: lightblue; color: black; }
        div { background-color: lightgrey; color: black; }
        table { table-layout: auto; width:100% border-collapse: collapse; }
        tr { display: flex; color: black; }
        th { display: flex; font-weight: bold; color: black; width: 200; text-align: right; }
        td { display: flex; text-align: left; }
      </style>
    </head>
    <body>
      <nav>
        <table>
          <tr itemscope="true" itemtype="http://schema.org/BreadcrumbList">
            <td itemscope="true" itemprop="itemListElement" itemtype="http://schema.org/ListItem">
              <a itemprop="item" href="https://esapulkkinen.github.io/cifl-math-library/">
                <b itemprop="name">Contents</b>
              </a>
              <meta itemprop="position" content="1"/>
            </td>
            <td itemscope="true" itemprop="itemListElement" itemtype="http://schema.org/ListItem">
              <a itemprop="item" href="https://github.com/esapulkkinen/cifl-math-library/">
                <b itemprop="name">Repository</b>
              </a>
              <meta itemprop="position" content="2"/>

            </td>
            <td itemscope="true" itemprop="itemListElement" itemtype="http://schema.org/ListItem">
              <a itemprop="item" href="https://esapulkkinen.github.io/cifl-math-library/doc-index.html">
                <b itemprop="name">Index</b>
              </a>
              <meta itemprop="position" content="3"/>
            </td>
          </tr>
        </table>
      </nav>
      
    <h1>Bibliography</h1>
    <div id="structured">
      <table frame="vsides" padding="5px" border-collapse="collapse">
      <xsl:for-each select="bibliography/CreativeWork|bibliography/Article|bibliography/Report|bibliography/Book|bibliography/ScholarlyArticle|bibliography/BlogPosting|bibliography/TechArticle|bibliography/DigitalDocument|bibliography/PublicationIssue|bibliography/SoftwareApplication|bibliography/WebPage|bibliography/WebSite|bibliography/SoftwareSourceCode|bibliography/Comment|bibliography/QAPage|bibliography/BlogPosting|bibliography/DigitalDocument">
        <xsl:sort select="summary"/>
        <tr itemprop="citation" itemscope="true">
          <xsl:attribute name="itemtype">http://schema.org/<xsl:value-of select="local-name()"/></xsl:attribute>
          <td colspan="2" width="100%">
            <details>
              <summary class="noselect"><xsl:value-of select="summary"/></summary>
              <table frame="vsides" padding="5px" border-collapse="collapse">
              <xsl:for-each select="field">
                <xsl:sort select="attribute::name"/>
                <xsl:call-template name="field"/>
              </xsl:for-each>
              </table>
            </details>
          </td>
        </tr>
      </xsl:for-each>
    </table>
    </div>
    </body>
    </html>
  </xsl:template>
  
  <xsl:template name="field">
    <tr flex="flex">
      <xsl:for-each select="@type">
        <xsl:attribute name="itemtype">http://schema.org/<xsl:value-of select="."/></xsl:attribute>
        <xsl:attribute name="itemscope">true</xsl:attribute>
      </xsl:for-each>
      <xsl:choose>
        <xsl:when test="summary">
          <xsl:attribute name="itemprop"><xsl:value-of select="@name"/></xsl:attribute>
        </xsl:when>
      </xsl:choose>
      <th><xsl:value-of select="@name"/>:</th>
      <xsl:choose>
        <xsl:when test="summary">
          <td width="100%">
            <details>
              <summary class="noselect"><xsl:attribute name="itemprop"><xsl:value-of select="summary/@name"/></xsl:attribute><xsl:value-of select="summary"/></summary>
              <table frame="vsides" padding="5px" border-collapse="collapse">
                <xsl:for-each select="field">
                  <xsl:call-template name="field"/>
                </xsl:for-each>
              </table>
            </details>
          </td>
        </xsl:when>
        <xsl:otherwise>
          <td><xsl:attribute name="itemprop"><xsl:value-of select="@name"/></xsl:attribute>
          <xsl:choose>
            <xsl:when test="@name='headline'">
              <cite><xsl:value-of select="node()"/></cite>
            </xsl:when>
            <xsl:when test="@name='url' or @name='doi'">
              <a>
                <xsl:attribute name="href"><xsl:value-of select="node()"/></xsl:attribute>
                <xsl:value-of select="node()"/>
              </a>
            </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="node()"/>
          </xsl:otherwise>
          </xsl:choose>
        </td>
        <xsl:if test="@type">
          <td>[<xsl:value-of select="@type"/>]</td>
        </xsl:if>
        </xsl:otherwise>
      </xsl:choose>
    </tr>
  </xsl:template>
</xsl:stylesheet>
