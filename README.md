# MaximumCity 
[![GitHub CI](https://github.com/ThreeEyedGod/MaximumCity/workflows/CI/badge.svg)](https://github.com/ThreeEyedGod/MaximumCity/actions) [![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](https://github.com/threeeyedgod/MaximumCity/blob/main/LICENSE)
[![Stable Version](https://img.shields.io/github/v/tag/ThreeEyedGod/MaximumCity)](https://img.shields.io/github/v/tag/ThreeEyedGod/MaximumCity)
[![Latest Release](https://img.shields.io/github/v/release/ThreeEyedGod/MaximumCity?color=%233D9970)](https://img.shields.io/github/v/release/ThreeEyedGod/MaximumCity?color=%233D9970)


The idea behind this project is that of an experimental ["serverless" computing service](https://www.protocol.com/newsletters/protocol-enterprise/serverless-container-aws?rebelltitem=1#rebelltitem1), that allows developers to write applications without having to know anything about the cloud hardware on which their applications will run -  often and also  referred to sometimes as "Function-as-a-Service" or Lambda. The application in this case is a mix of two sub-projects: the so-called Karatsuba algorithm, used for faster multiplication of large integers provided by a web user; and an unrelated sub-project extracting the IP address of the web user, co-relating it to a location, then corelating the location to an open weather API and then presenting a one-liner weather report. 

Synopsis
---------

MaximumCity is code built on Haskell. It comprises of a minimalist working program deployed to AWS lambda, whose workflows are managed by AWS Step Functions and accessible using AWS API Gateway. The development environment includes a CI pipeline using Github Actions. QuickTest and Hspec for unit testing.  The IDE is VSCode with Haskell for Visual Studio Code extension on a MacOS.

The intent was to gain a rapid understanding of a soup-to-nuts development cycle using VScode, Haskell, GitHub and Github-Actions for a cloud-hosted Function-as-a-Service aka Serverless environment.

### Update 
Branch Feature4 includes a Http-API* gateway to AWS Step Functions run synchronously (express workflows) which are used to run two parallel tasks. Apart from the old Karatsuba, the new functionality which runs in parallel is a weather sourcing task which uses caller's IP to get weather from a implementation of [DarkSky](https://twitter.com/alexanderrey007/status/1370733643279269889).

*That's what works with AWS Step functions - Synch. Workflows. No [Rest as of now](https://forums.aws.amazon.com/thread.jspa?messageID=973793). 
## The Program
Takes two numbers as inputs, multiplies them and sends back the result. 
Also grabs your IP address and determines the weather for that location from public weather sources. 

## ACKSHUALLY...
The program mutliplies two large numbers faster than the naive approach using the Karatsuba algorithm. The original inspiration was [this article in quanta magazine](https://www.quantamagazine.org/mathematicians-discover-the-perfect-way-to-multiply-20190411/). The clearest exposition of the Karatsuba algorithm can be found [here](http://people.cs.uchicago.edu/~laci/HANDOUTS/karatsuba.pdf). After an sub-optimal implementation of my own (can be seen in repo: start), I leveraged an existing fairly well-done [implementation by Steve Risto from several years back](https://github.com/Risto-Stevcev/haskell-karatsuba-multiplication). 

The weather part uses a reimplementaiton of DarkSky by [Alexander Rey](https://twitter.com/alexanderrey007/status/1370733643279269889)
## Try it out
Download the index.html under test folder/directory here into your local PC directory . To copy, click on index_updated.html, click Raw and then save as. Then open it in your localhost. Go give it a shot.
## Prerequisites
If you are looking to play around with the code, you will need

1. VSCode with Haskell extension (See #2, Good to have)
2. A Github account
3. Docker desktop for your OS
4. A Docker hub account
5. An AWS account (this will need a credit card)

## Good to have 
1. The [Fish](https://medium.com/tuannguyendotme/set-up-the-fish-shell-on-mac-step-by-step-6a77bcb2687c) [shell](https://www.youtube.com/watch?v=ux1SP9B5lSQ)
2. These other VScode extensions are useful ::: Bracket Pair Colorizer 2, Error Lens, Git Extension Pack, Git History (not sure if this comes along with the Git Extension pack), Haskell Syntax Highlighting, haskell-linter (this in turn needs hlint as a pre-req), Highlight matching Tag, Local History
3. You might need [AWS CLI](https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-welcome.html).  
## Dependencies
1. You will need to go install GHC, stack, cabal. Maybe [ghcup](https://www.haskell.org/ghcup/)
5. The [FP/CO docker image](https://docs.haskellstack.org/en/stable/maintainers/docker/) also [on/from Docker Hub](https://hub.docker.com/r/fpco/stack-build/). Also over [here on the haskell Tool Stack](https://docs.haskellstack.org/en/stable/docker_integration/)
## Building the Haskell i.e. Lambda program
Clone the repo. Fire up docker desktop. Run make in your local MaximumCity folder
```console
make
```
### Building AWS Step and Setting up the gateway
For now, AWS Step Function is configured right in the AWS console using AWS states language.
The API Gateway as well.


## AWS Lambda, API Gateway
1. First time: Upload function.zip and then use the lambda test capability to run a test. 
2. Next create a API Gateway AWS proxy based test and run it
3. Next add a trigger based on API Gateway
4. Use Curl and test
5. Cloudwatch->Cloudwatch Logs->Log Groups are helpful

## AWS Step Functions, API Gateway
This stuff is immature as hell especially when it comes to integration. There's lots of gotchas, lots of moving parts from IAM roles to policies to CORS.  In the absence of REST support in Q2 2021 for Synch. Workflows, one's left to work with the much touted http-api. There is no effective way to pass along standard headers or manipulate JSON as it moves from API GW to Step let alone further to Lambda (which is what Feature4 does). This particular implementation uses hacks in the API Gateway Integrations console - like hijacking unused integration fields to do some of that. Step Functions uses AWS States Language which is OK. Testing your step functions with the data flow similator is not always possible - especially if one is using incoming fields from gateway. AWS Cloudwatch is OK but needs more switches to be set so that the logs can be useful. The AWS API OpenAPI implementation leaves one exhausted. In order to run this Lambda, Lambda configuration - memory and timeout - needed to be jacked up. 
## CORS correction
So CORS is used to let another domain access your AWS API gateway and can be a bit of hell to get working. No different here. While this [25 minute v.d.o](https://www.youtube.com/watch?v=O3wWjWjvM6A) was useful, what had to be done was to change the response within the handler of my haskell program to spit out the right sets of CORS friedly response headers. Also, you would have to set "Enable cors" for your API under the gateway and also for each resource therein and click lambda integration under Integration -Post method. Don't forget to deploy and cross your toes. For Feature4 localhost needs to be enabled in Allow-Cross-Origin. Also you need to run index_updated.html from your apache or local web server.
# Related Resources
-----------------
## My Takeaway
About 90% of the "development" time was expended on troubleshooting issues with Haskell with AWS lambda, AWS Lambda itself, AWS API Gateway, testing between Lambda and API, between API and Step, CORS, Github Actions and JSON - in that order. About 10% with stack.yaml, package.yaml and GHC issues. The rest on the actual code and associated unit testing. 

## Serverless
The approach to run a Haskell program on AWS lambda (which apparently runs on a bare bones AmazonLinux runtime) is based on the [aws-lambda-haskell-runtime](https://hackage.haskell.org/package/aws-lambda-haskell-runtime) described [here](https://theam.github.io/aws-lambda-haskell-runtime/index.html). The following implementations which use the aforementioned library are useful reference implementations:
[A Haskell telegram bot on AWS Lambda](https://www.joachim-breitner.de/blog/770-A_Telegram_bot_in_Haskell_on_Amazon_Lambda) and [A Haskell Url Shortener on AWS Lambda](https://github.com/fcracker79/aws-lambda-haskell-url-shortener/)

Alternative approaches have been tried out [here](https://www.haskelltutorials.com/haskell-aws-lambda/) and [here](https://lazamar.github.io/deploying-statically-linked-haskell-to-lambda/)

## Github actions
Posts that served well include [this](https://blog.codingmilitia.com/2020/12/22/getting-started-with-github-actions/), [this](https://github.community/t/use-working-directory-for-entire-job/16747/8), [this](https://dev.to/shofol/run-your-github-actions-jobs-from-a-specific-directory-1i9e) and [this](https://www.theserverside.com/blog/Coffee-Talk-Java-News-Stories-and-Opinions/examples-GitHub-Actions-workflows)

This post on [GitHub to Docker](https://docs.docker.com/ci-cd/github-actions/) is worth reading. This [post on GitHub to AWS Lambda Deployment](https://github.com/marketplace/actions/aws-lambda-deploy) is useful as was this [other one](https://github.com/marketplace/actions/configure-aws-credentials-action-for-github-actions) for setting up AWS Credentials. AWS Lambda fileb:// command used at the very end of GIthub actions workflow file, presented challenges encountered by others [here](https://stackoverflow.com/questions/44684713/updating-aws-lambda-with-new-zip-file), [here](https://stackoverflow.com/questions/34362805/how-can-i-create-an-aws-lambda-function-using-the-aws-cli) and [here](https://docs.aws.amazon.com/cli/latest/userguide/cli-usage-parameters-file.html#cli-usage-parameters-file-binary)

**Note**: The user login to docker hub needed to be in lowercase for it to work correctly

## Foundational skills
To get going on Git, I recommend [this](https://www.youtube.com/watch?v=RGOj5yH7evk&feature=youtu.be&t=1375).  An awesome haskell resource is [Kowainik](https://kowainik.github.io/), their [blog](https://kowainik.github.io/posts). The [mini-patterns post](https://kowainik.github.io/posts/haskell-mini-patterns) and their [twitter handle](https://twitter.com/kowainik) are both recommended.  To ramp up on stack, go thru [this](https://schooloffp.co/2020/12/05/whirlwind-tour-of-stack-for-beginners.html#building-and-running-executables)

### Help ++
**Troubleshoot Haskell Builds**: This [older post](https://stackoverflow.com/questions/33446558/understanding-haskells-stack-program-and-the-resolver-and-lts-version) was helpful and of course the [Haskell Lts resolver site](https://www.stackage.org/lts-9.0). 

**AWS Lambda and AWS API driving you crazy?**: [This](https://blog.summercat.com/using-aws-lambda-and-api-gateway-as-html-form-endpoint.html). For a missing auth token check out [this](http://www.awslessons.com/2017/aws-api-gateway-missing-authentication-token/) and for internal server error, look [this up](https://aws.amazon.com/premiumsupport/knowledge-center/api-gateway-lambda-stage-variable-500/)

**Fine tune your .gitignore**: [Here](https://www.freecodecamp.org/news/gitignore-what-is-it-and-how-to-add-to-repo/)

**AWS Step Functions**
TBD

TO DO
---------------------------------------
Scores of basic things.

Increase Testing and test quality; Implement error handling which is currently non-existent; Implement haddock;   ~~Implement AWS API Gateway stages : test-staging-development~~ __DONE__; perhaps change to REST API; ~~Would be nice to have a basic website instead of having to use curl~~ __DONE__

Implement Refined types; Implement Code coverage and add a badge!, [Static Analysis](https://kowainik.github.io/projects/stan); Maybe a bit of [Logging](https://kowainik.github.io/projects/co-log); perhaps look into [Haskell bignum](https://iohk.io/en/blog/posts/2020/07/28/improving-haskells-big-numbers-support/)

An eventual aspirational goal would be get to [a Quantum Implementation of karatsuba](https://www.quantamagazine.org/a-new-approach-to-multiplication-opens-the-door-to-better-quantum-computers-20190424/). The spec is [here](https://arxiv.org/pdf/1904.07356.pdf) and [a parallel implementation standard here](https://arxiv.org/pdf/2009.14590.pdf)

Wrap up
---------------------------------------
Function as a Service aka Serverless seems like the [way to go](https://www.infoq.com/news/2021/01/bbc-serverless-scale/). Azure offers [it up as well](https://azure.microsoft.com/en-us/services/functions/). [Google too is on the game](https://cloud.google.com/functions/) specifically right-most [here](https://github.com/priyankavergadia/GCPSketchnote/blob/main/images/ComputeOptions.jpg).


I suspect the messiness of deploying with each cloud provider and associated gateways will ease out in a coupla years. One can see by trawling through stack overflow posts that it is already somewhat relatively simpler today compared to what it was in circa 2018. 