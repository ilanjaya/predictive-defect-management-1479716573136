<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@page import="java.io.BufferedWriter" %>
<%@page import="java.io.FileWriter" %>
<%@page import="java.util.Date" %>
<%@page import="java.util.Scanner" %>
<%
/** Log POSTs at / to a file **/
if ("POST".equalsIgnoreCase(request.getMethod())) {
        BufferedWriter writer = new BufferedWriter(new FileWriter("/tmp/sample-app.log", true));
        Scanner scanner = new Scanner(request.getInputStream()).useDelimiter("\\A");
	if(scanner.hasNext()) {
		String reqBody = scanner.next();
		writer.write(String.format("%s Received message: %s.\n", (new Date()).toString(), reqBody));
	}
        writer.flush();
        writer.close();
	
} else {
%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
  <!--
    
  -->
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
  <title>Welcome</title>
  <style>
  body {
    color: #ffffff;
    background-color: #c7c7c7;
    font-family: Arial, sans-serif;
    font-size:14px;
    -moz-transition-property: text-shadow;
    -moz-transition-duration: 4s;
    -webkit-transition-property: text-shadow;
    -webkit-transition-duration: 4s;
    text-shadow: none;
  }
  body.blurry {
    -moz-transition-property: text-shadow;
    -moz-transition-duration: 4s;
    -webkit-transition-property: text-shadow;
    -webkit-transition-duration: 4s;
    text-shadow: #fff 0px 0px 25px;
  }
  a {
    color: #0188cc;
  }
  .textColumn, .linksColumn {
    padding: 2em;
  }
  .textColumn {
    position: absolute;
    top: 0px;
    right: 50%;
    bottom: 0px;
    left: 0px;

    text-align: right;
    padding-top: 11em;
    background-color: #0188cc;
    background-image: -moz-radial-gradient(left top, circle, #6ac9f9 0%, #0188cc 60%);
    background-image: -webkit-gradient(radial, 0 0, 1, 0 0, 500, from(#6ac9f9), to(#0188cc));
  }
  .textColumn p {
    width: 75%;
    float:right;
  }
  .linksColumn {
    position: absolute;
    top:0px;
    right: 0px;
    bottom: 0px;
    left: 50%;

    background-color: #c7c7c7;
  }

  h1 {
    font-size: 500%;
    font-weight: normal;
    margin-bottom: 0em;
  }
  h2 {
    font-size: 200%;
    font-weight: normal;
    margin-bottom: 0em;
  }
  ul {
    padding-left: 1em;
    margin: 0px;
  }
  li {
    margin: 1em 0em;
  }

  </style>
</head>
<body id="sample">
  <p dir="ltr">&nbsp;</p>

<h1 dir="ltr" style="margin-left: 40px; text-align: center;"><strong><span style="font-size:28px;"><span style="color: rgb(105, 105, 105);"><span style="font-family: arial,helvetica,sans-serif;">Welcome to AIC</span></span></span></strong></h1>

<p dir="ltr"><img height="286" id="1479451934558_0" src="https://predictive-defect-management.mybluemix.net/Test_COE/soss-2016-homepage-banner.jpg" width="917" /></p>

<p dir="ltr" style="margin-left: 40px; text-align: center;">&nbsp;</p>

<p dir="ltr" style="margin-left: 40px; text-align: center;">&nbsp;</p>

<h3 dir="ltr" style="margin-left: 40px; text-align: center;"><span style="color:#696969;"><span style="font-family: arial,helvetica,sans-serif;"><span style="font-size: 20px;">Testing INDIA wiki</span></span></span><span style="color:#696969;"><span style="font-family: arial,helvetica,sans-serif;"><span style="font-size: 20px;"> - Your one stop shop to all answers on Testing as a service</span></span></span></h3>

<p dir="ltr">&nbsp;</p>

<p dir="ltr">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <img id="1479454175525_0" src="https://predictive-defect-management.mybluemix.net/Test_COE/delivery%20truck.png" />&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <img id="1479455141007_0" src="https://predictive-defect-management.mybluemix.net/Test_COE/sales-02.png" /></p>

<h3 dir="ltr"><span style="color:#696969;"><span style="font-size: 16px;"><span style="font-family: arial,helvetica,sans-serif;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; DELIVERY&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; SALES</span></span></span></h3>

<p dir="ltr">&nbsp;</p>

<p dir="ltr"><span style="font-size: 14px;"><span style="font-family: tahoma,geneva,sans-serif;">&nbsp;Click the icons&nbsp;below within Delivery or Sales&nbsp;to know more on your specific area of interest. Once you are within those areas, you can also navigate </span></span></p>

<p dir="ltr"><span style="font-size: 14px;"><span style="font-family: tahoma,geneva,sans-serif;">&nbsp;based on your current roles. </span></span></p>

<table border="0" dir="ltr" style="width: 921px;">
	<tbody>
		<tr>
			<td style="width: 495px;">
			<table border="0" style="width: 370px; height: 333px;">
				<tbody>
					<tr>
						<td style="width: 102.5px;">
						<p><a href="https://w3-connections.ibm.com/wikis/home?lang=en-us#!/wiki/GBS%20India%20Testing%20Services%20WIKI/page/Services" id="wikiLink1479304266087" page="Services" wiki="GBS India Testing Services WIKI"><img alt="" lconnwikimacro="image" lconnwikiparamattachmentname="services.png" lconnwikiparamwikipage="Welcome to AIC Testing Services India WIKI" src="https://predictive-defect-management.mybluemix.net/Test_COE/services.png" style="width: 83px; height: 83px;" /></a></p>

						<p>&nbsp;&nbsp;&nbsp;&nbsp;<strong> </strong></p>

						<p><strong>&nbsp;&nbsp;&nbsp;&nbsp; Services</strong></p>
						</td>
						<td style="width: 143.5px;">
						<p><a href="https://w3-connections.ibm.com/wikis/home?lang=en-us#!/wiki/GBS%20India%20Testing%20Services%20WIKI/page/Delivery%20-%20Assets" id="wikiLink1479366068868" page="Delivery - Assets" wiki="GBS India Testing Services WIKI"><img alt="" height="150" lconnwikimacro="image" lconnwikiparamattachmentname="assets.png" lconnwikiparamwikipage="Welcome to AIC Testing Services India WIKI" src="https://predictive-defect-management.mybluemix.net/Test_COE/assets.png" style="width: 104px; height: 79px;" width="171" /></a></p>

						<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Assets</strong></p>
						</td>
						<td style="width: 119.5px;">
						<p><a href="https://w3-connections.ibm.com/wikis/home?lang=en-us#!/wiki/GBS%20India%20Testing%20Services%20WIKI/page/Delivery%20-%20Education" id="wikiLink1479366068869" page="Delivery - Education" wiki="GBS India Testing Services WIKI"><img alt="" height="224" lconnwikimacro="image" lconnwikiparamattachmentname="Education.png" lconnwikiparamwikipage="Welcome to AIC Testing Services India WIKI" src="https://predictive-defect-management.mybluemix.net/Test_COE/Education.png" style="width: 109px; height: 95px;" width="225" /></a></p>

						<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<strong> Education </strong></p>
						</td>
					</tr>
					<tr>
						<td style="width: 102.5px;">
						<p><a href="https://w3-connections.ibm.com/wikis/home?lang=en-us#!/wiki/GBS%20India%20Testing%20Services%20WIKI/page/Delivery%20-%20Career" id="wikiLink1479366068870" page="Delivery - Career" wiki="GBS India Testing Services WIKI"><img alt="" height="417" lconnwikimacro="image" lconnwikiparamattachmentname="careers-icon.png" lconnwikiparamwikipage="Welcome to AIC Testing Services India WIKI" src="https://predictive-defect-management.mybluemix.net/Test_COE/careers-icon.png" style="width: 90px; height: 91px;" width="417" /></a></p>

						<p><strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Careers</strong></p>
						</td>
						<td style="width: 143.5px;">&nbsp;</td>
						<td style="width: 119.5px;">
						<p style="margin-left: 40px;"><a href="https://w3-connections.ibm.com/wikis/home?lang=en-us#!/wiki/GBS%20India%20Testing%20Services%20WIKI/page/Delivery%20-%20Contacts" id="wikiLink1479366068871" page="Delivery - Contacts" wiki="GBS India Testing Services WIKI"><img alt="" height="225" lconnwikimacro="image" lconnwikiparamattachmentname="contacts.png" lconnwikiparamwikipage="Welcome to AIC Testing Services India WIKI" src="https://predictive-defect-management.mybluemix.net/Test_COE/contacts.png" style="width: 76px; height: 80px;" width="224" /></a>&nbsp;&nbsp;&nbsp;<strong>Contacts</strong></p>
						</td>
					</tr>
				</tbody>
			</table>
			</td>
			<td style="width: 431px;">
			<table border="0" style="width: 428px; height: 177px;">
				<tbody>
					<tr>
						<td style="width: 384.5px;">
						<p style="margin-left: 200px;"><img alt="" height="224" lconnwikimacro="image" lconnwikiparamattachmentname="presales.png" lconnwikiparamwikipage="Welcome to AIC Testing Services India WIKI" src="https://predictive-defect-management.mybluemix.net/Test_COE/presales.png" style="width: 87px; height: 90px;" width="225" /></p>

						<p style="margin-left: 40px;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Pre Sales</strong></p>
						</td>
					</tr>
				</tbody>
			</table>

			<table border="0" style="width: 431px; height: 144px;">
				<tbody>
					<tr>
						<td style="width: 239px;">
						<p style="margin-left: 80px;"><img alt="" height="242" lconnwikimacro="image" lconnwikiparamattachmentname="solutioning.jpg" lconnwikiparamwikipage="Welcome to AIC Testing Services India WIKI" src="https://predictive-defect-management.mybluemix.net/Test_COE/solutioning.jpg" style="width: 98px; height: 95px;" width="208" /></p>

						<p style="margin-left: 40px;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Solutioning</strong></p>
						</td>
						<td style="width: 180px;">
						<p style="margin-left: 80px;"><img alt="" height="225" lconnwikimacro="image" lconnwikiparamattachmentname="references.png" lconnwikiparamwikipage="Welcome to AIC Testing Services India WIKI" src="https://predictive-defect-management.mybluemix.net/Test_COE/references.png" style="width: 108px; height: 115px;" width="225" /></p>

						<p style="margin-left: 40px;"><strong>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;References</strong></p>
						</td>
					</tr>
				</tbody>
			</table>
			</td>
		</tr>
	</tbody>
</table>

<p dir="ltr">&nbsp;</p>

<p dir="ltr" style="text-align: center;">&nbsp;</p>

<p dir="ltr" style="text-align: center;">&nbsp;</p>

<p dir="ltr" style="text-align: center;">&nbsp;</p>

<table border="0" dir="ltr" style="width: 917px; height: 28px;">
	<tbody>
		<tr>
			<td style="text-align: center; background-color: rgb(255, 160, 122);"><u><span style="font-family: lucida sans unicode,lucida grande,sans-serif;"><span style="font-size: 14px;"><strong>LeaderSpeak</strong></span></span></u></td>
			<td style="text-align: center; background-color: rgb(255, 160, 122);"><u><span style="font-family: lucida sans unicode,lucida grande,sans-serif;"><span style="font-size: 14px;"><strong>Share Ideas</strong></span></span></u></td>
			<td style="width: 203px; text-align: center; background-color: rgb(255, 160, 122);"><u><span style="font-family: lucida sans unicode,lucida grande,sans-serif;"><span style="font-size: 14px;"><strong>Let's Discuss</strong></span></span></u></td>
			<td style="width: 115px; text-align: center; background-color: rgb(255, 160, 122);"><u><span style="font-family: lucida sans unicode,lucida grande,sans-serif;"><span style="font-size: 14px;"><strong>Blog It!</strong></span></span></u></td>
			<td style="width: 208px; text-align: center; background-color: rgb(255, 160, 122);"><u><span style="font-family: lucida sans unicode,lucida grande,sans-serif;"><span style="font-size: 14px;"><strong>Newsmakers</strong></span></span></u></td>
		</tr>
	</tbody>
</table>

<p dir="ltr">&nbsp;</p>

<p dir="ltr">&nbsp;</p>
</script>
</body>
</html>
<% } %>
